(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let uncaught_exception_handler exn bt =
  (* XXX we want to use the actual installed one but we don't have access to
     it at the moment. Time for [Stdlib.Exn] to become a reality. *)
  Printexc.default_uncaught_exception_handler exn bt

(* Fibers *)

module Id = struct
  type t = int
  let nil = -1
  let equal = Int.equal
  let compare = Int.compare
  let create =
    let id = Atomic.make nil in
    fun () -> Atomic.incr id; Atomic.get id
end

module Id_map = Map.Make (Id)

(* The type for fiber states. Must be understood as follows:

   * In the [Running spawns] state the fiber may be executing, suspended or
     blocked. [spawns] keeps a reference on its spawns that may not be
     terminated yet.
   * In the [Aborting] state the fiber is in the process of aborting, if it
     executes it's for handling the [Abort] exception.
   * In the [Terminated _] state the fiber will never ever execute again. The
     [finally] function specified at spawn time (if any) has been executed.
*)

type 'a state =
| Running of e list ref (* Fiber's non-terminated spawns. *)
| Aborting
| Terminated of 'a option (* Final state, None is for aborted fibers. *)

and e = E : 'a t -> e (* existential *)
and 'a t =
  { e : e; (* existential to itself, premature optimization. *)
    id : Id.t; (* fiber unique id *)
    mutable state : 'a state; }

let make ?(state = Running (ref [])) () =
  let rec f = { e = E f; id = Id.create (); state } in f

let reap_terminated (E f as fiber) = match f.state with
| Terminated _ -> None | _ -> Some fiber

(* Effects *)

type spawn = { fiber : e; run : unit -> unit }
type 'a block = { block : e -> unit; abort : e -> unit; retv : e -> 'a }

type _ Effect.t +=
| Yield : unit Effect.t
| Spawn : spawn -> unit Effect.t
| Abort' : e option -> unit Effect.t
| Block : 'a block -> 'a Effect.t

let yield () = Effect.perform Yield

(* Identifiers *)

type id = Id.t
let id_nil = Id.nil
let id f = f.id

(* Aborting *)

exception Abort

let aborted () = make ~state:(Terminated None) ()
let self_abort () = Effect.perform (Abort' None); assert false
let abort f = match f.state with
| Terminated _ -> ()
| Aborting | Running _ -> Effect.perform (Abort' (Some f.e))

(* Return value

   XXX naïve "busy yielding" implementations. This needs to be improved
   by introducing waiter sets in the fibers or the scheduler. *)

let poll f = match f.state with
| Terminated v -> Some v
| Running _ | Aborting -> None

let rec join f = match f.state with
| Terminated v -> v
| Running _ | Aborting -> yield (); join f

let rec first f0 f1 = match f0.state with
| Terminated v -> Either.Left v
| Running _ | Aborting ->
    match f1.state with
    | Terminated v -> Either.Right v
    | Running _ | Aborting -> yield (); first f0 f1

let rec either f0 f1 = match f0.state with
| Terminated (Some v) -> abort f1; Some (Either.Left v)
| Terminated None | Aborting ->
    begin match join f1 with
    | None -> None
    | Some v -> Some (Either.Right v)
    end
| Running _ ->
    match f1.state with
    | Running _ -> yield (); either f0 f1
    | Terminated (Some v) -> abort f0; Some (Either.Right v)
    | Terminated None | Aborting ->
        yield ();
        match join f0 with
        | None -> None
        | Some v -> Some (Either.Left v)

(* Spawning *)

let handle_termination f ~finally func () =
  (* Wraps the fiber function [func] of fiber [f] to handle its termination.
     This is the only place that sets f.state to Terminated *)
  let terminate f ~finally v =
    let finally_no_exn ~finally = match finally with
    | None -> () | Some finally -> (* We follow Fun.protect here. *)
        try finally () with
        | exn ->
            let bt = Printexc.get_raw_backtrace () in
            uncaught_exception_handler (Fun.Finally_raised exn) bt
    in
    finally_no_exn ~finally; f.state <- Terminated v
  in
  let ensure_abort f ~finally = match abort f with
  | exception Abort -> terminate ~finally f None
  | () -> (* Someone swallowed the Abort exception *) terminate ~finally f None
  | exception (Stack_overflow | Out_of_memory | Sys.Break as e) ->
      let bt = Printexc.get_raw_backtrace () in
      terminate f ~finally None;
      Printexc.raise_with_backtrace e bt
  | exception exn -> (* Bad, a cleanup handler messed up *)
      let bt = Printexc.get_raw_backtrace () in
      uncaught_exception_handler exn bt;
      terminate ~finally f None
  in
  let abort_termination f ~finally = match f.state with
  | Aborting -> terminate f ~finally None
  | Running _ -> (* Abort raised by a client, let's pretend. *)
      ensure_abort f ~finally
  | Terminated _ -> assert false
  in
  let uncaught_termination f ~finally exn bt =
    uncaught_exception_handler exn bt;
    match f.state with
    | Running _ -> ensure_abort f ~finally
    | Aborting -> (* Bad, a cleanup handler messed up. *)
        terminate f ~finally None
    | Terminated _ -> assert false
  in
  let value_termination f ~finally v = match f.state with
  | Aborting -> (* Someone swallowed the Abort exception. *)
      terminate f ~finally None
  | Running spawns ->
      let rec wait_spawns spawns = (* XXX busy yielding again *)
        spawns := List.filter_map reap_terminated !spawns;
        if !spawns = [] then () else (yield (); wait_spawns spawns)
      in
      begin match wait_spawns spawns with
      | exception Abort -> abort_termination ~finally f
      | exception (Stack_overflow | Out_of_memory | Sys.Break as e) ->
          let bt = Printexc.get_raw_backtrace () in
          terminate f ~finally None;
          Printexc.raise_with_backtrace e bt
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          uncaught_termination f ~finally exn bt
      | () -> terminate f ~finally (Some v)
      end
  | Terminated _ -> assert false
  in
  match f.state with
  | Aborting -> (* We didn't even start [func] *) terminate f ~finally None
  | Running _ ->
      begin match func () with
      | v -> value_termination f ~finally v
      | exception Abort -> abort_termination ~finally f
      | exception (Stack_overflow | Out_of_memory | Sys.Break as e) ->
          let bt = Printexc.get_raw_backtrace () in
          terminate f ~finally None;
          Printexc.raise_with_backtrace e bt
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          uncaught_termination f ~finally exn bt
      end
  | Terminated _ -> assert false

let spawn ?finally func =
  let f = make () in
  let spawn = { fiber = f.e; run = handle_termination ~finally f func } in
  Effect.perform (Spawn spawn);
  f

(* Blocking *)

module E = struct
  type t = e
  let id (E f) = f.id
  let equal (E f0) (E f1) = Id.equal f0.id f1.id
  let compare (E f0) (E f1) = Id.compare f0.id f1.id
  let attach (E f) ~spawn = match f.state with
  | Terminated _ | Aborting -> assert false
  | Running spawns ->
      (* cleanup a bit, this avoids the list to grow unbounded on "server"
         fiber that spawn on new "connections". *)
      let spawns' = List.filter_map reap_terminated !spawns in
      spawns := spawn :: spawns'

  let active_spawns (E f) = match f.state with
  | Terminated _ | Aborting -> [] | Running spawns -> !spawns

  let set_aborting (E f) = match f.state with
  | Terminated _ -> () | _ -> f.state <- Aborting

  let state (E f) = match f.state with
  | Terminated _ -> `Terminated | Running _ -> `Running | Aborting -> `Aborting

  let main ?finally func =
    let main = make () in
    main, handle_termination main ~finally func
end

let block ~block ~abort ~retv = Effect.perform (Block { block; abort; retv })

type unblock = poll:bool -> E.t option

(* Built-in scheduler (Fiber.run) *)

(* Circular doubly linked list *)

module Clist = struct
  type 'a t =
    { mutable v : 'a option; (* None is for the root. *)
      mutable prev : 'a t; (* on root this points to last element. *)
      mutable next : 'a t; (* on root this points to the first element. *) }

  let root () = let rec root = { v = None; next = root; prev = root } in root
  let make_first root n =
    n.next.prev <- n.prev; n.prev.next <- n.next;
    n.next <- root.next; n.prev <- root;
    root.next.prev <- n; root.next <- n

  let add_first root v =
    let n = { v = Some v; prev = root; next = root.next } in
    root.next.prev <- n; root.next <- n; n

  let add_last root v =
    let n = { v = Some v; prev = root.prev; next = root } in
    root.prev.next <- n; root.prev <- n; n

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
end

type scheduler =
  { mutable current : e;
    (* A fiber is never in [s.todo] and [s.blocked] at the same time. *)
    todo : (e * (unit -> unit)) Clist.t;
    mutable blocked : (e * (unit -> unit)) Id_map.t;
    unblock : unblock; }

let current s = s.current
let set_current s f = s.current <- f
let find_todo s f = Clist.take ~sat:(fun (f', _) -> E.equal f f') s.todo
let has_blocked s = not (Id_map.is_empty s.blocked)
let add_blocked s f b = s.blocked <- Id_map.add f.id (f.e, b) s.blocked
let schedule_first s work = ignore (Clist.add_first s.todo work)
let schedule_last s work = ignore (Clist.add_last s.todo work)

let rec schedule_aborts s (E f) =
  (* We "reschedule to first" in depth first pre-order of [f] and
     its non-terminated descendents. *)
  let reschedule_abort_to_first s f = match Id_map.find_opt f.id s.blocked with
  | Some work ->
      s.blocked <- Id_map.remove f.id s.blocked;
      schedule_first s work
  | None ->
      match find_todo s f.e with
      | Some work -> schedule_first s work
      | None -> assert false
  in
  match f.state with
  | Running spawns ->
      f.state <- Aborting;
      reschedule_abort_to_first s f;
      List.iter (schedule_aborts s) !spawns
  | Aborting | Terminated _ -> ()

let schedule_unblocked s (E f) = match f.state with
| Running _ ->
    let schedule_and_remove = function
    | Some work -> schedule_last s work; None
    | None ->
        invalid_arg
          (Printf.sprintf "unblock returned non blocked fiber %d" f.id)
    in
    s.blocked <- Id_map.update f.id schedule_and_remove s.blocked
| Aborting (* block.abort may not have executed yet *) -> ()
| Terminated _ -> assert false

let resume (E fiber) k () = match fiber.state with
| Aborting -> Effect.Deep.discontinue k Abort
| Running _ -> Effect.Deep.continue k ()
| Terminated _ -> assert false

let resume_blocked (E fiber as f) block k () = match fiber.state with
| Aborting ->
    begin match block.abort f with
    | () -> Effect.Deep.discontinue k Abort
    | exception (Stack_overflow | Out_of_memory | Sys.Break as e) ->
        Effect.Deep.discontinue k e
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        uncaught_exception_handler exn bt;
        Effect.Deep.discontinue k Abort
    end
| Running _ ->
    begin match block.retv f with
    | v -> Effect.Deep.continue k v
    | exception exn -> Effect.Deep.discontinue k exn
    end
| Terminated _ -> assert false

let rec exec_next_todo s () =
  let rec schedule_unblocked_fibers s = match s.unblock ~poll:true with
  | None -> () | Some f -> schedule_unblocked s f; schedule_unblocked_fibers s
  in
  schedule_unblocked_fibers s;
  match Clist.take_first s.todo with
  | Some (fiber, k) -> set_current s fiber; k ()
  | None when not (has_blocked s) -> ()
  | None ->
      match s.unblock ~poll:false with
      | Some f -> schedule_unblocked s f; exec_next_todo s ()
      | None ->
          (* We could end up busy waiting here, let's relax. *)
          Domain.cpu_relax ();
          exec_next_todo s ()

let make_scheduler ?unblock current =
  let todo = Clist.root () and blocked = Id_map.empty in
  match unblock with
  | Some unblock -> {current; todo; blocked; unblock}
  | None ->
      let rec s = { current; todo; blocked; unblock }
      and unblock ~poll:_ = match Id_map.choose_opt s.blocked with
      | None -> None
      | Some (_, (e, _)) -> schedule_aborts s e; Some e
      in
      s

(* Effect handlers *)

let do_spawn s exec { fiber; run } k =
  E.attach (current s) ~spawn:fiber;
  schedule_last s (fiber, fun () -> exec s run);
  Effect.Deep.continue k ()

let do_yield s k =
  let (E f as fiber) = current s in
  match f.state with
  | Running _ ->
      schedule_last s (fiber, (resume fiber k));
      exec_next_todo s ()
  | Aborting (* Don't let it yield *) -> Effect.Deep.discontinue k Abort
  | Terminated _ -> assert false

let do_abort s what k =
  let current = current s in
  let fiber = match what with None -> current | Some fiber -> fiber in
  (* Schedule: if [current] is being aborted, will throw Abort in [k]
     once its spawns are aborted. If another fiber [fiber] is being aborted,
     will resume once [fiber] is terminated. *)
  schedule_first s (current, (resume current k));
  schedule_aborts s fiber;
  exec_next_todo s ()

let do_block s block k =
  let (E f as fiber) = current s in
  match f.state with
  | Running _ ->
      begin match block.block fiber with
      | exception exn -> Effect.Deep.discontinue k exn
      | () ->
          add_blocked s f (resume_blocked fiber block k);
          exec_next_todo s ()
      end
  | Aborting (* Don't block but invoke [block]'s abort *) ->
      resume_blocked fiber block k ()
  | Terminated _ -> assert false

type 'a handler = ('a, unit) Effect.Deep.continuation -> unit

let run ?unblock ?finally func =
  let rec exec : scheduler -> (unit -> unit) -> unit = fun s f ->
    let retc = exec_next_todo s in
    let exnc = raise in
    let effc (type c) (e : c Effect.t) = match e with
    | Yield -> Some (do_yield s : c handler)
    | Spawn spawn -> Some (do_spawn s exec spawn : c handler)
    | Abort' what -> Some (do_abort s what : c handler)
    | Block block -> Some (do_block s block : c handler)
    | e -> None
    in
    Effect.Deep.match_with f () { Effect.Deep.retc; exnc; effc }
  in
  let main, run = E.main ?finally func in
  let s = make_scheduler ?unblock main.e in
  exec s run;
  assert (not (has_blocked s));
  match main.state with
  | Running _ | Aborting -> assert false | Terminated r -> r

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
