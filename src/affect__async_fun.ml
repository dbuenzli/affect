(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect__base
open Affect__action.Private

let fun_nop () = ()

let pp_id_num ppf num =
  Format.fprintf ppf "@<0>\x1B[%dm%04x@<0>\x1B[0m" (30 + (num mod 8)) num

let pp_call_id ppf id = Format.fprintf ppf "λ%a" pp_id_num id
let pp_cancelled_call_id ppf id = Format.fprintf ppf "∅%a" pp_id_num id
let meta_call_id = Action.Meta.Key.make ~pp_value:pp_call_id ()

module Async_fun = struct
  module Call_handler = struct
    type t = { handle : 'a. (unit -> 'a) -> 'a }
    let call h f = h.handle f
    let none = { handle = function f -> f () }
    let compose h0 h1 =
      if h1 == none then h0 else
      if h0 == none then h1 else
      { handle = fun f -> h0.handle (fun () -> h1.handle f) }

    let of_list hs = List.fold_right compose hs none
  end

  module Schedule = struct
    type priority = Low | Normal | High
    let priority_to_string = function Normal -> "-" | Low -> "↓" | High -> "↑"
    let pp_priority ppf p = Format.pp_print_string ppf (priority_to_string p)

    type t = { only_main : bool ; priority : priority option; }
    let make ?(only_main = false) ?priority () = { only_main; priority }
    let only_main sched = sched.only_main
    let priority sched = sched.priority
    let default = make ()
  end

  type 'a id = 'a Type.Id.t
  type 'a blocked_get = 'a Action.Blocked.t
  type blocked_action_invoke = I : 'a Action.Blocked.t -> blocked_action_invoke
  type blocked_wait_cancelled = Action.Blocked.Value.t
  type cancellation = Marked | Unmarked of blocked_wait_cancelled list

  type notify_parent = unit -> unit
  type 'a notify =
    { finish : 'a t -> unit; (* mainly used for tracing *)
      parent : notify_parent; (* called when the function finished. *) }

  and 'a state =
  | Running of (* Result is being computed *)
      { notify : 'a notify;
        cancellation : cancellation;
        gets : 'a blocked_get list (* these are interested in our result *) }
  | Result_delayed of (* Result computed, waiting on children *)
      { notify : 'a notify;
        cancellation : cancellation;
        gets : 'a blocked_get list;
        result : 'a Action.Result.t; }
  | Result of (* Result computed and all children returned *)
      { cancellation_mark : bool;
        result : 'a Action.Result.t }

  and call = V : 'a t -> call [@@unboxed]
  and 'a t =
    { id : 'a id;
      handler : Call_handler.t; (* Has the parent handler composed in *)
      schedule : Schedule.t;
      priority : Schedule.priority Atomic.t;
      parent : call; (* [none] on the root call created by the scheduler *)
      running_children : running_children;
      mask_cancellation : bool Atomic.t;
      blocked_action_invoke : blocked_action_invoke Atomic.t;
      state : 'a state Atomic.t; }

  and running_children =
    { count : int Atomic.t; (* count of those that are still unfinished *)
      calls : call Atomic.t list Atomic.t;
      calls_length : int Atomic.t; (* length of [calls] list *) }

  let make_running_children () =
    { count = Atomic.make 0; calls = Atomic.make [];
      calls_length = Atomic.make 0; }

  let blocked_action_invoke_none = I (Action.Blocked.none ())
  let is_blocked_action_invoke_none b =
    Repr.phys_equal b blocked_action_invoke_none

  let init_notify = { finish = (fun _ -> ()); parent = fun_nop }
  let init_cancellation = Unmarked []
  let init_state ~notify ~cancellation =
    Running { notify; cancellation; gets = [] }

  let init_state_default () =
    init_state ~notify:init_notify ~cancellation:init_cancellation

  let rec none =
    { id = Type.Id.make (); handler = Call_handler.none;
      schedule = Schedule.default; priority = Atomic.make Schedule.Normal;
      parent = V none; running_children = make_running_children ();
      mask_cancellation = Atomic.make false;
      blocked_action_invoke = Atomic.make blocked_action_invoke_none;
      state = Atomic.make (init_state_default ()) }

  let call_none = none.parent

  (* Properties *)

  let id af = af.id
  let id_int af = Type.Id.uid af.id
  let handler af = af.handler
  let schedule af = af.schedule
  let priority af = Atomic.get af.priority
  let set_priority af p = Atomic.set af.priority p
  let only_main af = Schedule.only_main af.schedule
  let is_none af = Int.equal (id_int none) (id_int af)
  let state af = Atomic.get af.state
  let is_cancellation_masked af = Atomic.get af.mask_cancellation
  let is_cancelled af = match state af with
  | Result { cancellation_mark } -> cancellation_mark
  | Running { cancellation = Marked }
  | Result_delayed { cancellation = Marked } -> true
  | _ -> false

  let result af = match Atomic.get af.state with
  | Result { result } -> Some result | Running _ | Result_delayed _ -> None

  module Running_children : sig
    type t = running_children
    val is_empty : t -> bool
    val calls : t -> call Atomic.t list
    val clear : t -> unit
    val add : t -> call -> notify_parent
  end = struct
    (* Manages the list of running children of a function. For structured
       concurrency we can do just with a counter of running children. But for
       cancellation we need a actual reference on the running children for
       propagating the cancellation mark.

       A function cannot [call] in parallel, only deletions from the list are
       made in parallel by children. So we let the parent be the only mutator of
       this list and have children in references in the list. A child sets
       its reference to [call_none] and decreases the count when it's done via
       a closure that is stored in the [notify.parent] field of the function's
       state. We keep track of the length of the list and gc the [call_none]
       references if it grows too large for these long running functions that
       keep on calling asynchronous functions (e.g. server loops). *)
    type t = running_children

    let is_empty r = Atomic.get r.count = 0
    let calls r = Atomic.get r.calls
    let clear r = assert (Atomic.get r.count = 0); Atomic.set r.calls []
    let gc_threshold = 300
    let gc r =
      let is_live call_ref =
        let dead = Repr.phys_equal (Atomic.get call_ref) call_none in
        (if dead then Atomic.decr r.calls_length); not dead
      in
      let live_calls = List.filter is_live (Atomic.get r.calls) in
      Atomic.set r.calls live_calls

    let maybe_gc r =
      if Atomic.get r.calls_length - Atomic.get r.count > gc_threshold then gc r

    let add r child =
      maybe_gc r;
      let call_ref = Atomic.make child in
      Atomic.incr r.count; Atomic.incr r.calls_length;
      Atomic.set r.calls (call_ref :: Atomic.get r.calls);
      (* The notify_parent for the child: *)
      (fun () -> Atomic.set call_ref call_none; Atomic.decr r.count)
  end

  (* Making *)

  let make_with_state ~handler:h ~schedule ~parent:(V p as parent) state =
    let handler = Call_handler.compose (handler p) h in
    let priority = match Schedule.priority schedule with
    | None -> priority p | Some priority -> priority
    in
    let priority = Atomic.make priority in
    let running_children = make_running_children () in
    let mask_cancellation = Atomic.make false in
    let blocked_action_invoke = Atomic.make blocked_action_invoke_none in
    let state = Atomic.make state in
    { id = Type.Id.make (); handler; schedule; priority; parent;
      running_children; mask_cancellation; blocked_action_invoke; state; }

  let make
      ?(notify_finish = fun _ -> ()) ~handler ~schedule ~parent:(V p as parent)
      ()
    =
    let state = init_state_default () in
    let af = make_with_state ~handler ~schedule ~parent state in
    let state =
      let notify_parent =
        if is_none p (* only true for root function *) then fun_nop else
        Running_children.add p.running_children (V af)
      in
      let notify = { finish = notify_finish; parent = notify_parent } in
      let cancellation = if is_cancelled p then Marked else init_cancellation in
      init_state ~notify ~cancellation
    in
    Atomic.set af.state state;
    af

  let make_with_result result =
    let handler = Call_handler.none and schedule = Schedule.default in
    let parent = call_none in
    let state = Result { cancellation_mark = false; result } in
    make_with_state ~handler ~schedule ~parent state

  let from_val v = make_with_result (Action.Result.Value v)
  let from_exn exn bt = make_with_result (Action.Result.Exn (exn, bt))

  (* The recursive function definition below implements structured concurrency.

     1. When we create a function with [make] we add it in the
        [running_children] field of its parent.
     2. After running the function body with [run] we call [finish_run].
        If the function still has [running_children] then we move to the
        [Result_delayed] state or otherwise the [Result] state can be set
        and [finish] is called.
     3. When a function moves to the [Result] state it removes itself from the
        [running_children] of its parent via the [notify.parent] it carried
        in its state and tries to [finish] the parent.

     One thing to keep in mind is that when a function has moved to
     [Result_delayed], its [running_children.count] can only decrease as the
     function no longer has a body to call new asynchronous functions. *)

  let cancellation_mark = function Marked -> true | Unmarked _ -> false
  let has_returned af = match state af with Result _ -> true | _ -> false
  let has_running_children af =
    not (Running_children.is_empty af.running_children)

  let call_has_returned (V af) = has_returned af
  let call_has_running_children (V af) = has_running_children af

  let rec finish : 'a.
    'a t -> notify:'a notify -> 'a Action.Result.t ->
    'a Action.Blocked.t list -> unit
    =
    fun af ~notify candidate gets ->
    (* The justification for the order here is that we want [notify.finish]
       before the unblocks since it makes tracing more understandable.
       We notify the parent (a kind of implicit get) before the [gets] as it
       gives it a chance to finish by itself and if it is in the [gets] itself
       it would still have the fun in its [running_children] despite the [get]
       unblocks, harmless but bizzare. *)
      Running_children.clear af.running_children;
      notify.finish af;
      notify.parent ();
      List.iter (Action.Blocked.synced_unblock ~candidate) (List.rev gets);
      try_finish af.parent

  and try_finish call =
    let rec loop ~backoff (V af as call) =
      if call_has_running_children call then () else match state af with
      | Result _ | Running _ -> ()
      | Result_delayed { notify; cancellation; result; gets} as seen ->
          let cancellation_mark = cancellation_mark cancellation in
          let state = Result { cancellation_mark; result } in
          if Atomic.compare_and_set af.state seen state
          then finish af ~notify result gets
          else loop ~backoff:(Atomic.Backoff.once backoff) call
    in
    loop ~backoff:Atomic.Backoff.default call

  let finish_run ?(force = false) af result =
    let rec loop ~backoff af result = match state af with
    | Result _ -> if not force then assert false else ()
    | Result_delayed delayed as seen ->
        if not force then assert false else
        let state = Result_delayed { delayed with result } in
        if Atomic.compare_and_set af.state seen state
        then try_finish (V af)
        else loop ~backoff:(Atomic.Backoff.once backoff) af result
    | Running { notify; cancellation; gets } as seen ->
        if has_running_children af then
          let delayed = Result_delayed { notify; cancellation; gets; result } in
          if Atomic.compare_and_set af.state seen delayed
          then try_finish (V af)
          else loop ~backoff:(Atomic.Backoff.once backoff) af result
        else
        let cancellation_mark = cancellation_mark cancellation in
        let state = Result { cancellation_mark; result } in
        if Atomic.compare_and_set af.state seen state
        then finish af ~notify result gets
        else loop ~backoff:(Atomic.Backoff.once backoff) af result
    in
    loop ~backoff:Atomic.Backoff.default af result

  let run af f =
    let handle_f () = Call_handler.call af.handler f in
    finish_run af (Action.Result.of_fun_run handle_f)

  (* get action *)

  let get_poll af ~continue = match state af with
  | Result r -> Some (continue r.result) | Running _ | Result_delayed _ -> None

  let get_block af ~blocked:get =
    let rec loop ~backoff af get = match state af with
    | Result { result } -> Action.Blocked.synced_unblock ~candidate:result get
    | Result_delayed ({ gets } as delayed) as seen ->
        let keep = Action.Blocked.is_not_synced in
        let gets = List.filter keep (get :: gets) in
        let state = Result_delayed { delayed with gets } in
        if Atomic.compare_and_set af.state seen state then () else
        loop ~backoff:(Atomic.Backoff.once backoff) af get
    | Running ({ gets } as running) as seen ->
        let keep = Action.Blocked.is_not_synced in
        let gets = List.filter keep (get :: gets) in
        let state = Running { running with gets } in
        if Atomic.compare_and_set af.state seen state then () else
        loop ~backoff:(Atomic.Backoff.once backoff) af get
    in
    loop ~backoff:Atomic.Backoff.default af get

  let get_meta af =
    let bindings = Action.Meta.[Binding (meta_call_id, (id_int af))] in
    Action.Meta.make ~name:"Fun.Async.get" ~bindings ()

  let get' af =
    let poll = get_poll af and block = get_block af in
    let meta = get_meta af in
    Action.Primitive.make ~meta ~poll ~block

  (* get functions *)

  let get af = match state af with
  | Result { result } -> Action.Result.return result
  | Running _ | Result_delayed _ -> Action.invoke (get' af)

  let get_all afs = List.map get afs
  let get_either af0 af1 = Action.(invoke (either (get' af0) (get' af1)))

  (* Cancellation *)

  exception Cancelled

  let cancelled_exn_result () =
    let bt = Printexc.get_raw_backtrace () in
    Action.Result.Exn (Cancelled, bt)

  let do_cancel_unblocks af ~waits =
    let I b as i =
      Atomic.exchange af.blocked_action_invoke blocked_action_invoke_none
    in
    if not (is_blocked_action_invoke_none i) then begin
      let candidate = cancelled_exn_result () in
      Action.Blocked.synced_unblock ~candidate b
    end;
    List.iter Action.Blocked.Value.synced_unblock (List.rev waits)

  let set_cancel_mark af =
    let rec loop ~backoff af = match state af with
    | Result _ -> () (* cancellation state is immutable at that point *)
    | Result_delayed delayed as seen ->
        begin match delayed.cancellation with
        | Marked -> ()
        | Unmarked waits ->
            let state = Result_delayed { delayed with cancellation = Marked } in
            if Atomic.compare_and_set af.state seen state
            then do_cancel_unblocks af ~waits
            else loop ~backoff:(Atomic.Backoff.once backoff) af
        end
    | Running running as seen ->
        begin match running.cancellation with
        | Marked -> ()
        | Unmarked waits ->
            let state = Running { running with cancellation = Marked } in
            if Atomic.compare_and_set af.state seen state
            then do_cancel_unblocks af ~waits
            else loop ~backoff:(Atomic.Backoff.once backoff) af
        end
    in
    loop ~backoff:Atomic.Backoff.default af

  let cancel af =
    (* The order is depth-first pre-order. depth vs breadth could be pondered
       but pre-order seems less debatable so that the cancel propagates from
       the root to the leaves. *)
    let rec loop calls todo = match calls with
    | call :: calls ->
        let V af = Atomic.get call in
        if is_none af then loop calls todo else begin
          set_cancel_mark af;
          loop (Running_children.calls af.running_children) (calls :: todo)
        end
    | [] -> match todo with current :: todo -> loop current todo | [] -> ()
    in
    loop [Atomic.make (V af)] []

  let do_sys_break (V af) =
    cancel af; finish_run ~force:true af (cancelled_exn_result ())

  (* wait_cancelled action *)

  let wait_cancelled_poll af tag ~continue =
    if is_cancelled af then None else Some (continue (Action.Result.Value tag))

  let wait_cancelled_block af tag ~blocked:wait =
    let rec loop ~backoff af wait = match state af with
    | Result { cancellation_mark = marked } ->
        if marked then Action.Blocked.Value.synced_unblock wait else ()
    | Result_delayed delayed as seen ->
        begin match delayed.cancellation with
        | Marked -> Action.Blocked.Value.synced_unblock wait
        | Unmarked waits ->
            let keep = Action.Blocked.Value.is_not_synced in
            let cancellation = Unmarked (List.filter keep (wait :: waits)) in
            let state = Result_delayed { delayed with cancellation } in
            if Atomic.compare_and_set af.state seen state then () else
            loop ~backoff:(Atomic.Backoff.once backoff) af wait
        end
    | Running running as seen ->
        begin match running.cancellation with
        | Marked -> Action.Blocked.Value.synced_unblock wait
        | Unmarked waits ->
            let keep = Action.Blocked.Value.is_not_synced in
            let cancellation = Unmarked (List.filter keep (wait :: waits)) in
            let state = Running { running with cancellation } in
            if Atomic.compare_and_set af.state seen state then () else
            loop ~backoff:(Atomic.Backoff.once backoff) af wait
        end
    in
    let wait = Action.Blocked.Value.make tag wait in
    loop ~backoff:Atomic.Backoff.default af wait

  let wait_cancelled_meta af =
    let bindings = Action.Meta.[Binding (meta_call_id, (id_int af))] in
    Action.Meta.make ~name:"Fun.Async.wait_cancelled" ~bindings ()

  let wait_cancelled af tag =
    let poll = wait_cancelled_poll af tag in
    let block = wait_cancelled_block af tag in
    let meta = wait_cancelled_meta af in
    Action.Primitive.make ~meta ~poll ~block

  (* unblock_action_invoke bare primitive action for {!Action.Invocation}

     Note that formally we could just have used wait_cancelled but that would
     add a waiter in Running on every Action.invoke, so we special case it *)

  let unblock_action_invoke_poll af ~continue =
    if is_cancelled af then Some (continue (cancelled_exn_result ())) else None

  let unblock_action_invoke_block af ~blocked =
    (* First publish the fact that we are blocking to avoid races on
       the call to [is_cancelled] if a parallel [cancel] happens. *)
    Atomic.set af.blocked_action_invoke (I blocked);
    if not (is_cancelled af) then () else begin
      (* Note, this is harmless if we were synced by a parallel [cancel].  *)
      Atomic.set af.blocked_action_invoke blocked_action_invoke_none;
      let candidate = cancelled_exn_result () in
      Action.Blocked.synced_unblock ~candidate blocked
    end

  let unblock_action_invoke' af =
    let poll = unblock_action_invoke_poll af in
    let block = unblock_action_invoke_block af in
    let meta = wait_cancelled_meta af in
    Action.Primitive.make_bare ~meta ~poll ~block

  let unblock_action_invoke (V af) =
    if is_cancellation_masked af then None else Some (unblock_action_invoke' af)

  (* Effects *)

  type 'a Effect.t +=
  | Call : Call_handler.t * Schedule.t * (unit -> 'a) -> 'a t Effect.t
  | Yield : unit Effect.t
  | Get_current_call : call Effect.t
  | Get_parallel_count : int Effect.t

  let yield () = Effect.perform Yield
  let get_current_call () = Effect.perform Get_current_call

  let call ?(handler = Call_handler.none) ?(schedule = Schedule.default) f =
    Effect.perform (Call (handler, schedule, f))

  let trap_exn exn =
    let bt = Printexc.get_raw_backtrace () in
    if exn = Cancelled then () else
    if not (Exn.is_runtime_system exn) then Exn.trap exn bt else
    Printexc.raise_with_backtrace exn bt

  let call_trap_exn' ?handler ?schedule f =
    let f () = try f () with exn -> trap_exn exn in
    call ?handler ?schedule f

  let call_trap_exn ?handler ?schedule f =
    ignore (call_trap_exn' ?handler ?schedule f)

  let cancel_current () = let V af = get_current_call () in cancel af
  let is_current_cancelled () = let V a = get_current_call () in is_cancelled a
  let check_cancellation () = if is_current_cancelled () then raise Cancelled
  let mask_cancellation f =
    let V a = get_current_call () in
    let finally () = Atomic.set a.mask_cancellation false in
    Fun.protect ~finally @@ fun () -> Atomic.set a.mask_cancellation true; f ()

  let protect ~finally f =
    let finally () = mask_cancellation finally in Fun.protect ~finally f

  (* Existential *)

  module Call = struct
    type id = int
    let id (V f) = id_int f
    let none_id = id call_none
    let pp_none_id ppf () = Format.pp_print_string ppf "<none>"
    let pp_id ppf id =
      if id = none_id then pp_none_id ppf () else pp_call_id ppf id

    type 'a async_fun = 'a t
    type t = call
    let current () = Effect.perform Get_current_call
    let none = call_none

    let parent (V af) = af.parent
    let schedule (V af) = af.schedule
    let priority (V af) = Atomic.get af.priority

    let cancel (V af) = cancel af
    let is_cancelled (V af) = is_cancelled af
    let wait_cancelled (V af) tag = wait_cancelled af tag

    let equal (V af0) (V af1) = Int.equal (id_int af0) (id_int af1)
    let compare (V af0) (V af1) = Int.compare (id_int af0) (id_int af1)
    let is_none f = equal f call_none
    let is_only_main (V af) = Schedule.only_main af.schedule
    let has_returned (V af) = has_returned af

    let of_async_fun f = V f
    let to_async_fun : type a. a Type.Id.t -> t -> a async_fun option =
      fun tid (V af) -> match Type.Id.provably_equal tid af.id  with
      | None -> None
      | Some Type.Equal -> Some af

    let pp ppf c =
      let id = id c in
      if id = none_id then pp_none_id ppf () else
      if is_cancelled c then pp_cancelled_call_id ppf id else pp_call_id ppf id
  end

  let pp ppf af = Call.pp ppf (V af)

  (* Dividing work *)

  let err_count n = Printf.sprintf "count %d must be strictly positive" n
  let parallel_count () = Effect.perform Get_parallel_count
  let parallel_count_override count =
    let handle f =
      if count < 1 then invalid_arg (err_count count) else
      match f () with
      | v -> v
      | effect Get_parallel_count, k -> Effect.Deep.continue k count
    in
    Call_handler.{ handle }

  let parallel_worker_count () = Int.max 1 (parallel_count () - 1)
  let divide_work = Affect__base.divide_work
end

include Async_fun

module Private = struct
  module Async_fun = Async_fun
end
