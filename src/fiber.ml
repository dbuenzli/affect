(*---------------------------------------------------------------------------
   Copyright (c) 2024 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Preliminaries *)

let pp_uchar ppf u = Format.fprintf ppf "@<1>%s" u
let pp_list ?(pp_sep = Format.pp_print_space) pp_v ppf l =
  Format.pp_print_list ~pp_sep pp_v ppf l

let debug fmt = Format.eprintf (fmt ^^ "@.")

let invalid_argf fmt = Format.kasprintf invalid_arg fmt
let is_asynchronous_exn = function
| Stack_overflow | Out_of_memory | Sys.Break -> true | _ -> false

module Circular_list = struct (* Circular doubly linked list *)
  type 'a t =
    { mutable v : 'a option; (* None is for the root. *)
      mutable prev : 'a t; (* on root this points to last element. *)
      mutable next : 'a t; (* on root this points to the first element. *) }

  let make () = let rec root = { v = None; next = root; prev = root } in root

  let make_first root n =
    n.next.prev <- n.prev; n.prev.next <- n.next;
    n.next <- root.next; n.prev <- root;
    root.next.prev <- n; root.next <- n

  let add_first root v =
    let n = { v = Some v; prev = root; next = root.next } in
    root.next.prev <- n; root.next <- n

  let add_last root v =
    let n = { v = Some v; prev = root.prev; next = root } in
    root.prev.next <- n; root.prev <- n

  let take_first root =
    let first = root.next in
    root.next <- first.next; first.next.prev <- root; first.v

  let take_last root =
    let last = root.prev in
    root.prev <- last.prev; last.prev.next <- root; last.v

  let take ~sat root = (* O(n) *)
    let rec loop pred n = match n.v with
    | None -> None
    | Some v when sat v -> n.next.prev <- n.prev; n.prev.next <- n.next; n.v
    | Some _ -> loop pred n.next
    in
    loop pred root.next

  let of_list vs = let l = make () in List.iter (add_last l) vs; l
end

(* Fiber ids *)

module Id = struct
  type t = int
  let nil = 0
  let equal = Int.equal
  let compare = Int.compare
  let pp ppf id = Format.fprintf ppf "%03d" id
  let make =
    let id = Atomic.make (nil + 1) in
    fun () -> Atomic.fetch_and_add id 1
end

module Id_set = Set.Make (Id)

(* Core fiber model definitions.

   XXX the datastructures and the set of effects can likely be streamlined
   we went for straightforward implementation for now. It needs more thinking
   that has been done here especially for being friendly to a multi domain
   work stealing scheduler. *)

exception Cancelled

module Private = struct
  type priority = Low | Normal | High
  type handle = V : 'a fiber -> handle [@@unboxed] (* existential fiber *)
  and 'a fiber =
    { id : Id.t; (* fiber unique id *)
      only_main : bool; (* If [true] only schedule on the main thread. *)
      priority : priority; (* hint, must be refined by prio of awaited_by *)
      mutable cancelled : bool; (* only ever moves from [false] to [true]. *)
      mutable non_cancelling_blocks : bool; (* no cancel notif. on blocks *)
      mutable awaited_by : Id_set.t; (* Possibly still awaited by these. *)
      mutable still_in_scope : handle list; (* to await before returning *)
      mutable returns : ('a, exn * Printexc.raw_backtrace) Either.t; }

  type await =
    { mutable fibers : handle list; (* at least one of these will return *)
      all : bool; (* return only all when all return *) }

  type 'a async =
    { only_main : bool; (* force scheduling on the main thread. *)
      func_priority : priority option; (* None is the caller's priority. *)
      func : unit -> 'a (* function to run asynchronously *) }

  type 'a block =
    { block : handle -> unit; (* called to register the blocking fiber *)
      cancel : handle -> bool; (* called if cancelled during the block *)
      return : handle -> 'a (* called to return from the block *) }

  type _ Effect.t +=
  | Async : 'a async -> 'a fiber Effect.t (* Request an async fun call *)
  | Await : await -> unit Effect.t (* Await on fibers *)
  | Block : 'a block -> 'a Effect.t (* Block the executing fiber *)
  | Cancel : handle option -> unit Effect.t (* Cancel a fiber *)
  | Return : unit Effect.t (* Notify the executing fiber can return or raise *)
  | Self : handle Effect.t (* Ask for the executing fiber *)
  | Yield : unit Effect.t (* Cooperatively suspend the executing fiber *)

  let running = (* Stub value tested for physical equality by is_running *)
    let exception N in Either.Right (N, Printexc.get_raw_backtrace ())

  let is_running f = f.returns == running
  let is_cancelled_exn f = match f.returns with
  | Either.Right (Cancelled, _) -> true | _ -> false

  (* Formatters *)

  let pp_any_value ppf v = Format.fprintf ppf "<%a>" pp_uchar "abstr"
  let pp_cancelled ppf c = pp_uchar ppf (if c then "∅" else "φ")
  let pp_priority ppf p =
    pp_uchar ppf (match p with Normal -> " " | Low -> "↓" | High -> "↑")

  let pp_value pp_v ppf f =
    if is_running f then pp_uchar ppf "⟳" else match f.returns with
    | Either.Left v -> Format.fprintf ppf "%a" pp_v v
    | Either.Right (e, _) -> Format.pp_print_string ppf (Printexc.to_string e)

  let pp_id ppf f =
    Format.fprintf ppf "[%a%a%a]"
      pp_cancelled f.cancelled pp_priority f.priority Id.pp f.id

  let pp' pp_v ppf f =
    Format.fprintf ppf "@[<1>[%a%a%a %a]@]"
      pp_cancelled f.cancelled pp_priority f.priority Id.pp f.id
      (pp_value pp_v) f

  let pp ppf f = pp' pp_any_value ppf f

  module Handle = struct
    type t = handle = V : 'a fiber -> t [@@unboxed]
    let self () = Effect.perform Self
    let id (V f) = f.id
    let priority (V f) = f.priority
    let cancelled (V f) = f.cancelled
    let is_running (V f) = is_running f
    let is_cancelled_exn (V f) = is_cancelled_exn f
    let equal (V f0) (V f1) = Id.equal f0.id f1.id
    let compare (V f0) (V f1) = Id.compare f0.id f1.id
    let list fs = (* Technically Fun.id *) List.map (fun f -> (V f)) fs
    let pp ppf (V f) = pp ppf f
  end

  (* Fibers *)

  let id f = f.id
  let priority f = f.priority
  let cancelled f = f.cancelled
  let handle f = V f

  let make_fiber ~only_main ~cancelled ~priority =
    { id = Id.make (); only_main; priority; cancelled;
      non_cancelling_blocks = false; awaited_by = Id_set.empty;
      still_in_scope = []; returns = running; }

  let do_fiber_return f = (* Returns the result of [f] after it ended *)
    assert (not (is_running f));
    match f.returns with
    | Either.Left v -> v
    | Either.Right (exn, bt) -> Printexc.raise_with_backtrace exn bt

  let attach_fiber ~scope:(V f) (V sub as sub') =
    (* This associates sub to the scope of fiber [f]. *)
    assert (is_running f); assert (is_running sub);
    (* Pruning avoids the list to grow unbounded on long running fibers.
       This is because fibers do not have a reference on their parent
       to remove themselves from still_in_scope. XXX Perhaps we should
       consider adding a parent field to fibers to avoid any gc problem of
       long running parents holding on terminated fibers for too long. *)
    let prune (V f as f') = if is_running f then Some f' else None in
    sub.cancelled <- f.cancelled;
    f.still_in_scope <- sub' :: List.filter_map prune f.still_in_scope

  let run_scope f func () =
    (* Wraps fiber [f]'s body [func] for execution. It implements
       the structured concurrency aspect. *)
    let finish_scope f =
      if f.still_in_scope <> []
      then Effect.perform (Await { fibers = f.still_in_scope; all = true });
      f.still_in_scope <- [];
    in
    match func () with
    | v -> finish_scope f; f.returns <- Left v; Effect.perform Return
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        if is_asynchronous_exn exn then Printexc.raise_with_backtrace exn bt;
        finish_scope f; f.returns <- Right (exn, bt); Effect.perform Return
end

(* Fibers *)

include Private

type 'a t = 'a fiber

let async ?(only_main = false) ?priority:func_priority func =
  Effect.perform (Async {only_main; func_priority; func})

let from_val v = async (Fun.const v)
let yield () = Effect.perform Yield
let poll f = if is_running f then None else Some (do_fiber_return f)

(* Cancelling *)

let cancel f =
  if not f.cancelled && is_running f then Effect.perform (Cancel (Some (V f)))

let self_cancel () = Effect.perform (Cancel None)
let self_is_cancelled () = let V f = Effect.perform Self in f.cancelled
let self_check_cancellation () = if self_is_cancelled () then raise Cancelled

(* Awaiting *)

let invalid_empty_list () = invalid_arg "Cannot await an empty list of fibers"

let await f =
  if is_running f then Effect.perform (Await {fibers = [V f]; all = true});
  do_fiber_return f

let await_all fs =
  Effect.perform (Await {fibers = Handle.list fs; all = true});
  List.map do_fiber_return fs

let await_first fs =
  Effect.perform (Await {fibers = Handle.list fs; all = false});
  let rec loop acc = function
  | [] -> invalid_empty_list ()
  | f :: fs ->
      if is_running f then loop (f :: acc) fs else
      let v = do_fiber_return f in (* N.B. may raise *)
      v, List.rev_append acc fs
  in
  loop [] fs

let await_either f0 f1 =
  Effect.perform (Await {fibers = [V f0; V f1]; all = false});
  if is_running f1
  then Either.Left (do_fiber_return f0)
  else Either.Right (do_fiber_return f1)

(* Picking *)

let rec pick_first fs =
  if fs = [] then invalid_empty_list () else
  begin
    Effect.perform (Await {fibers = Handle.list fs; all = false});
    let rec loop acc = function
    | [] ->
        if acc = [] then raise Cancelled else
        pick_first (List.rev acc)
    | f :: fs ->
        if is_running f then loop (f :: acc) fs else
        if is_cancelled_exn f then loop acc fs else
        let fs = List.rev_append acc fs in
        let () = List.iter cancel fs in
        do_fiber_return f (* N.B. may raise *)
    in
    loop [] fs
  end

let pick_either f0 f1 =
  Effect.perform (Await {fibers = [V f0; V f1]; all = false});
  if not (is_running f0) then begin
    if is_cancelled_exn f0
    then Either.Right (await f1)
    else (cancel f1; Either.Left (do_fiber_return f0))
  end else begin
    assert (not (is_running f1));
    if is_cancelled_exn f1
    then Either.Left (await f0)
    else (cancel f0; Either.Right (do_fiber_return f1))
  end

(* Blocking *)

let block ~block ~cancel ~return =
  Effect.perform (Block { block; cancel; return })

let self_non_cancelling_blocks func =
  let V f = Effect.perform Self in
  let before = f.non_cancelling_blocks in
  let finally () = f.non_cancelling_blocks <- before in
  f.non_cancelling_blocks <- true;
  Fun.protect ~finally func

type unblock = poll:bool -> Handle.t option

let never_unblock ~poll:_ = None
let unblocks = function
| [] -> never_unblock
| us ->
    let count = List.length us in
    let us = Circular_list.of_list us in
    fun ~poll -> (* FIXME doesn't work for [poll:false] *)
      let rec find_first rem_to_check us =
        if rem_to_check = 0 then None else
        let u = Circular_list.take_first us |> Option.get in
        match u ~poll with
        | None -> Circular_list.add_last us u; find_first (rem_to_check - 1) us
        | Some _ as ret -> Circular_list.add_last us u; ret
      in
      find_first count us

(* Trapping exceptions *)

let trap_user_exn f = async @@ fun () -> try Ok (await f) with
| exn ->
    let bt = Printexc.get_raw_backtrace () in
    if is_asynchronous_exn exn || exn = Cancelled
    then Printexc.raise_with_backtrace exn bt else Error (exn, bt)

let trap_cancelled f = async @@ fun () -> try Some (await f) with
| Cancelled -> None

let trap_any_exn f = async @@ fun () -> try Ok (Some (await f)) with
| Cancelled -> Ok None
| exn ->
    let bt = Printexc.get_raw_backtrace () in
    if is_asynchronous_exn exn
    then Printexc.raise_with_backtrace exn bt
    else Error (exn, bt)

(* Built-in scheduler *)

module Scheduler = struct

  (* Single domain for now. *)

  module Id_map = Map.Make (Id)

  type blocked =
  | Block : 'a block * ('a, unit) Effect.Deep.continuation -> blocked
  | Await : Handle.t * await * (unit, unit) Effect.Deep.continuation -> blocked

  type t =
    { mutable current : handle;
      (* A fiber is never in [s.todo] and [s.blocked] at the same time. *)
      todo : (handle * (unit -> unit)) Circular_list.t;
      mutable blocked :
        (* This won't work cheaply with multiple domains, both cancelling
           and awating needs to know where a given fiber is blocked. *)
        blocked Id_map.t;
      unblock : unblock; }

  let make ?(domains = Domain.recommended_domain_count ()) ~unblock current =
    { current; todo = Circular_list.make (); blocked = Id_map.empty; unblock }

  let current s = s.current
  let set_current s f = s.current <- f
  let has_blocked s = not (Id_map.is_empty s.blocked)
  let add_block s f b k =
    s.blocked <- Id_map.add (Handle.id f) (Block (b, k)) s.blocked

  let add_await s f a k =
    s.blocked <- Id_map.add (Handle.id f) (Await (f, a, k)) s.blocked

  let resume f k () = Effect.Deep.continue k ()
  let resume_with_exn f k exn () = Effect.Deep.discontinue k exn
  let resume_block f block k () =
    assert (Handle.is_running f);
    match block.return f with
    | v -> Effect.Deep.continue k v
    | exception exn -> Effect.Deep.discontinue k exn

  let schedule_first s work = ignore (Circular_list.add_first s.todo work)
  let schedule_last s work = ignore (Circular_list.add_last s.todo work)
  let schedule_unblocked_block s f =
    assert (Handle.is_running f);
    let schedule_and_remove = function
    | Some (Block (block, k)) ->
        schedule_last s (f, (resume_block f block k)); None
    | _ ->
        invalid_argf
          "unblock function error: returned a non blocked fiber %a"
          Id.pp (Handle.id f)
    in
    s.blocked <- Id_map.update (Handle.id f) schedule_and_remove s.blocked

  let schedule_block_cancel_raise s f exn k =
    s.blocked <- Id_map.remove (Handle.id f) s.blocked;
    schedule_last s (f, (resume_with_exn f k exn))

  let schedule_blocked_awaiting s (V finished) =
    let schedule_blocked_await id = match Id_map.find_opt id s.blocked with
    | Some (Await (((V af as af'), await, k))) ->
        if not await.all then begin
          let update_awaited_by (V f) =
            f.awaited_by <- Id_set.remove af.id f.awaited_by;
          in
          List.iter update_awaited_by await.fibers;
          s.blocked <- Id_map.remove id s.blocked;
          schedule_last s (af', (resume af k));
        end else begin
          let not_id (V f) = not (Id.equal f.id finished.id) in
            await.fibers <- List.filter not_id await.fibers;
            if await.fibers <> [] then () else
            (s.blocked <- Id_map.remove id s.blocked;
             schedule_last s (af', (resume af k)));
          end
      | None -> ()
      | Some (Block _) -> assert false
    in
    Id_set.iter schedule_blocked_await finished.awaited_by;
    finished.awaited_by <- Id_set.empty

  let rec schedule_cancel_scope s (V f) =
    (* We propogate in depth first order. *)
    f.cancelled <- true;
    if f.non_cancelling_blocks then () else
    begin match Id_map.find_opt f.id s.blocked with
    | Some (Block (block, k)) ->
        begin match block.cancel (V f) with
        | false -> ()
        | true -> schedule_block_cancel_raise s (V f) Cancelled k
        | exception exn ->
            let bt = Printexc.get_raw_backtrace () in
            if is_asynchronous_exn exn
            then Printexc.raise_with_backtrace exn bt
            else schedule_block_cancel_raise s (V f) exn k
        end
    | None | Some (Await (_, _, _)) -> ()
    end;
    List.iter (schedule_cancel_scope s) f.still_in_scope

  let rec exec_next_todo s () =
    let rec schedule_unblocked_fibers s = match s.unblock ~poll:true with
    | None -> ()
    | Some f -> schedule_unblocked_block s f; schedule_unblocked_fibers s
    in
    schedule_unblocked_fibers s;
    match Circular_list.take_first s.todo with
    | Some (fiber, k) -> set_current s fiber; k ()
    | None when not (has_blocked s) -> ()
    | None ->
        match s.unblock ~poll:false with
        | Some f -> schedule_unblocked_block s f; exec_next_todo s ()
        | None ->
            (* We could end up busy waiting here, let's relax. *)
            Domain.cpu_relax ();
            exec_next_todo s ()

  (* Effect handlers *)

  type 'a handler = ('a, unit) Effect.Deep.continuation -> unit

  let do_async s exec { only_main; func_priority = prio; func } k =
    let current = current s in
    let priority = Option.value ~default:(Handle.priority current) prio in
    let cancelled = (Handle.cancelled current) in
    let f = make_fiber ~only_main ~cancelled ~priority in
    let run_scope = run_scope f func in
    attach_fiber ~scope:current (V f);
    schedule_last s (V f, fun () -> exec s run_scope);
    Effect.Deep.continue k f

  let do_yield s k =
    let current = current s in
    schedule_last s (current, (resume current k));
    exec_next_todo s ()

  let do_cancel s what k =
    let current = current s in
    let f = match what with None -> current | Some fiber -> fiber in
    if not (Handle.cancelled f) then schedule_cancel_scope s f;
    schedule_first s (current, (resume current k));
    exec_next_todo s ()

  let do_block s block k =
    let current = current s in
    match block.block current with
    | exception exn -> Effect.Deep.discontinue k exn
    | () -> add_block s current block k; exec_next_todo s ()

  let do_await s await k =
    let current = current s in
    let rec loop await acc = function
    | (V f as f') :: fs when is_running f -> loop await (f' :: acc) fs
    | f :: fs (* f returned *) -> if await.all then loop await acc fs else None
    | [] -> if acc = [] then None else Some { await with fibers = acc }
    in
    begin match loop await [] await.fibers with
    | None -> (* no need to block *)
        schedule_first s (current, (resume current k))
    | Some await ->
        let update_awaited_by (V f) =
          f.awaited_by <- Id_set.add (Handle.id current) f.awaited_by;
        in
        let () = List.iter update_awaited_by await.fibers in
        let await = (Await (current, await, k)) in
        s.blocked <- Id_map.add (Handle.id current) await s.blocked
    end;
    exec_next_todo s ()

  let do_self s k = Effect.Deep.continue k s.current
  let do_return s k =
    let current = current s in
    schedule_blocked_awaiting s current;
    schedule_first s (current, (resume current k));
    exec_next_todo s ()

  let run ?domains ~unblock func =
    let rec exec : t -> (unit -> unit) -> unit = fun s f ->
      let retc = exec_next_todo s in
      let exnc = raise in
      let effc (type c) (e : c Effect.t) = match e with
      | Yield -> Some (do_yield s : c handler)
      | Async async -> Some (do_async s exec async : c handler)
      | Cancel what -> Some (do_cancel s what : c handler)
      | Block block -> Some (do_block s block : c handler)
      | Await await -> Some (do_await s await : c handler)
      | Self -> Some (do_self s : c handler)
      | Return -> Some (do_return s : c handler)
      | e -> None
      in
      Effect.Deep.match_with f () { Effect.Deep.retc; exnc; effc }
    in
    let main = make_fiber ~only_main:true ~cancelled:false ~priority:Normal in
    let scope = run_scope main func in
    let s = make ~unblock ?domains (V main) in
    exec s scope;
    assert (not (has_blocked s));
    assert (not (is_running main));
    do_fiber_return main
end

let main ?domains ~unblock f =
  let domains = match domains with
  | Some _ as d -> d
  | None ->
      let var = Sys.getenv_opt "AFFECT_DOMAIN_COUNT" in
      Option.join @@ (Option.map int_of_string_opt var)
  in
  Scheduler.run ?domains ~unblock f
