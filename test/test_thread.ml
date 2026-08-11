(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Affect

(* Shows that actions and/or asynchronous functions can be used with
   system threads. Perhaps we could expose some of the stuff in an
   affect.thread library.

   Also shows how they can interact with a running Fun.Async.main in all
   sorts of funny ways.

   There is still stuff that should likely not be done e.g. running
   threads on the domains of the [Fun.Async.main] scheduler like is done below.
   If the thread blocks without releasing the runtime lock you block that
   scheduler's subdomain. *)

module Thread_blocker = struct
  type t = { m : Mutex.t; unblock : Condition.t; mutable block : bool; }
  let make () =
    { m = Mutex.create (); unblock = Condition.create (); block = false }

  let enable b = Mutex.protect b.m @@ fun () -> b.block <- true
  let unblock b =
    Mutex.protect b.m @@ fun () ->
    b.block <- false; Condition.signal b.unblock

  let block ?(unless = fun _ -> false) b =
    Mutex.protect b.m @@ fun () ->
    if unless ()
    then b.block <- false
    else while b.block do Condition.wait b.unblock b.m done
end

module Actionable_thread : sig
  val create : (unit -> unit) -> Thread.t
  (** [create f] is a thread running [f ()] in which actions can be invoked.
      If they block they sleep the thread. *)
end = struct
  open Action.Private

  type t =
    { id : Fun.Async.Trace.exec_id;
      blocker : Thread_blocker.t; }

  let make () =
    let blocker = Thread_blocker.make () in
    let id = Fun.Async.Trace.current_exec_id () in
    { blocker; id }

  let report_trace b t = Fun.Async.Trace.report (b.id, t)

  let do_action_invoke a action k = match Action.Invocation.make action with
  | Either.Left (exn, bt) -> Effect.Deep.discontinue_with_backtrace k exn bt
  | Either.Right invocation ->
      let continue = Action.Result.deep_continue_thunk k in
      match Action.Invocation.do_poll invocation ~continue with
      | Some continue -> continue ()
      | None ->
          let call_id = Fun.Async.Call.none_id in
          let meta = Action.Invocation.meta invocation in
          let r = ref Action.Result.none in
          let unblock result =
            (* XXX Would be nice to have the scheduler's id if there is one *)
            let exec_id = Fun.Async.Trace.current_exec_id () in
            Fun.Async.Trace.report (exec_id, Action_unblock (call_id, meta));
            r := result;
            Thread_blocker.unblock a.blocker
          in
          Thread_blocker.enable a.blocker;
          report_trace a (Action_block (call_id, meta));
          Action.Invocation.do_block ~unblock invocation;
          Thread_blocker.block a.blocker;
          continue !r ()

  let run actionable f = match f () with
  | () -> ()
  | effect Action.Invoke action, k -> do_action_invoke actionable action k
  | effect Action.Invoke_poll action, k -> Action.do_invoke_poll action k

  let handle_actions f = run (make ()) f
  let create f = Thread.create handle_actions f
end

module Asyncable_thread : sig
  val create :
    ?handler:Fun.Async.Call_handler.t -> ?schedule:Fun.Async.Schedule.t ->
    (unit -> unit) -> Thread.t
  (** [create_asyncable f] is a thread running [f ()] as root asynchronous
      function. *)
end = struct
  open Action.Private
  open Fun.Async.Private

  type ready = Fun.Async.Call.t * (unit -> unit)
  type t =
    { id : Fun.Async.Trace.exec_id;
      b : Thread_blocker.t;
      ready_queue : ready Queue.t;
      blocked : int ref;
      mutable current : Fun.Async.Call.t; }

  let make () =
    let id = Fun.Async.Trace.current_exec_id () in
    let b = Thread_blocker.make () and ready_queue = Queue.create () in
    let blocked = ref 0 and current = Fun.Async.Call.none in
    { id; b; ready_queue; blocked; current }

  let report_trace s t = Fun.Async.Trace.report (s.id, t)
  let incr_blocked s = incr s.blocked
  let decr_blocked s = decr s.blocked
  let none_blocked s = !(s.blocked) = 0
  let has_ready s = Queue.length s.ready_queue <> 0
  let schedule_ready s ready = Queue.add ready s.ready_queue
  let rec run_next_ready s = match Queue.take_opt s.ready_queue with
  | Some (call, continue) -> s.current <- call; continue ()
  | None ->
      if none_blocked s then () else begin
        Thread_blocker.enable s.b;
        Thread_blocker.block ~unless:(fun () -> none_blocked s) s.b;
        run_next_ready s
      end

  let do_action_invoke s action k =
    let unblock = Async_fun.unblock_action_invoke s.current in
    match Action.Invocation.make ?unblock action with
    | Either.Left (exn, bt) -> Effect.Deep.discontinue_with_backtrace k exn bt
    | Either.Right invocation ->
        let continue = Action.Result.deep_continue_thunk k in
        match Action.Invocation.do_poll invocation ~continue with
        | Some continue -> continue ()
        | None ->
            let call = s.current in
            let call_id = Fun.Async.Call.id s.current in
            let meta = Action.Invocation.meta invocation in
            let unblock result =
              (* XXX Would be nice to have the scheduler's ids *)
              let exec_id = Fun.Async.Trace.current_exec_id () in
              Fun.Async.Trace.report (exec_id, Action_unblock (call_id, meta));
              schedule_ready s (call, continue result);
              decr_blocked s;
              Thread_blocker.unblock s.b
            in
            s.current <- Fun.Async.Call.none;
            incr_blocked s;
            report_trace s (Action_block (call_id, meta));
            Action.Invocation.do_block ~unblock invocation;
            run_next_ready s

  let do_action_invoke_poll s action k =
    let unblock = Async_fun.unblock_action_invoke s.current in
    Action.do_invoke_poll ?unblock action k

  let do_yield s k =
    let call = s.current in
    let ready = call, fun () -> Effect.Deep.continue k () in
    s.current <- Fun.Async.Call.none;
    report_trace s (Async_fun_yield (Async_fun.Call.id call));
    schedule_ready s ready;
    run_next_ready s

  let do_async_call s ~handler ~schedule run f =
    let parent = s.current in
    let af = Async_fun.make ~handler ~schedule ~parent () in
    let run_f () = run s (fun () -> Async_fun.run af f) in
    let parent = Async_fun.Call.id parent and id = Async_fun.id_int af in
    report_trace s (Async_fun_call { parent; id });
    schedule_ready s (Fun.Async.Call.of_async_fun af, run_f); af

  let do_sys_break s =
    let call = s.current in
    s.current <- Fun.Async.Call.none;
    Async_fun.do_sys_break call;
    run_next_ready s

  let rec run s f = match f () with
  | () -> if has_ready s then run_next_ready s else () (* the end *)
  | effect Async_fun.Call (handler, schedule, f), k ->
      Effect.Deep.continue k @@ do_async_call s ~handler ~schedule run f
  | effect Action.Invoke act, k -> do_action_invoke s act k
  | effect Action.Invoke_poll act, k -> do_action_invoke_poll s act k
  | effect Async_fun.Yield, k -> do_yield s k
  | effect Async_fun.Get_current_call, k -> Effect.Deep.continue k s.current
  | effect Async_fun.Get_parallel_count, k -> Effect.Deep.continue k 1
  | exception Sys.Break -> do_sys_break s

  let handle_async_funs
      ?(handler = Fun.Async.Call_handler.none)
      ?(schedule = Fun.Async.Schedule.default) f
    =
    let s = make () in
    ignore (do_async_call s ~handler ~schedule run f);
    try run s (fun () -> ()) with
    | exn -> Fun.Async.trap_exn exn

  let create ?handler ?schedule f =
    Thread.create (handle_async_funs ?handler ?schedule) f
end

let domain_count = Test_common.domain_count

(* These tests must work reliably. *)

let test_inter_thread_ping_pong =
  Test.test "Ping-pong between threads" @@ fun () ->
  let port = Port.make () in
  let t0 =
    Actionable_thread.create @@ fun () ->
    Test.string (Port.take port) "ping";
    Port.offer port "pong";
  in
  let t1 =
    Actionable_thread.create @@ fun () ->
    Port.offer port "ping";
    Test.string (Port.take port) "pong";
  in
  Thread.join t0; Thread.join t1

let test_thread_asyncable =
  Test.test "Single threaded asynchronous function scheduler" @@ fun () ->
  let t =
    Asyncable_thread.create @@ fun () ->
    let port = Port.make () in
    let _f0 = Fun.Async.call @@ fun () ->
      Test.string (Port.take port) "ping";
      Port.offer port "pong"
    in
    let f1 = Fun.Async.call @@ fun () ->
      Port.offer port "ping";
      Test.string (Port.take port) "pong";
    in
    Fun.Async.get f1;
    let f2 = Fun.Async.call @@ fun () -> Port.take port (* blocks *) in
    Fun.Async.cancel f2;
    Test.raises Fun.Async.Cancelled (fun () -> Fun.Async.get f2);
    ()
  in
  Thread.join t;
  ()

let test_thread_affect_ping_pong =
  Test.test' domain_count "Ping-pong between thread and affect" @@
  fun domain_count ->
  let port = Port.make () in
  let t =
    Actionable_thread.create @@ fun () ->
    Test.string (Port.take port) "ping";
    Port.offer port "pong";
  in
  begin Fun.Async.main ?domain_count @@ fun () ->
    Port.offer port "ping";
    let f = Fun.Async.call @@ fun () -> Port.take port in
    Test.string (Fun.Async.get f) "pong";
  end;
  Thread.join t;
  ()

let test_thread_affect_first_class_ping_pong =
  Test.test' domain_count
    "Ping-pong between thread and affect with first-class action" @@
  fun domain_count ->
  let invoke_that_action = Port.make () in
  let respond = Port.make () in
  let t =
    Actionable_thread.create @@ fun () ->
    let action = Port.take invoke_that_action in
    Test.string (Action.invoke action) "ping";
    Port.offer respond "pong"
  in
  begin Fun.Async.main ?domain_count @@ fun () ->
    let unblock = Cell.Once.make () in
    let f = Fun.Async.call @@ fun () -> Cell.Once.get unblock; "ping" in
    Port.offer invoke_that_action (Fun.Async.get' f);
    Fun.Async.yield ();
    Cell.Once.try_set unblock ();
    Fun.Async.yield ();
    Test.string (Port.take respond) "pong";
  end;
  Thread.join t;
  ()

let test_thread_asyncable =
  Test.test' domain_count "Ping-pong between asyncable thread and affect" @@
  fun domain_count ->
  let port = Port.make () in
  let t =
    Asyncable_thread.create @@ fun () ->
    let forward = Port.make () in
    let _f = Fun.Async.call @@ fun () -> Port.offer forward (Port.take port) in
    let r = Fun.Async.call @@ fun () -> Port.take forward in
    Test.string (Fun.Async.get r) "ping";
    Port.offer port "pong"
  in
  begin Fun.Async.main ?domain_count @@ fun () ->
    Port.offer port "ping";
    let f = Fun.Async.call @@ fun () -> Port.take port in
    Test.string (Fun.Async.get f) "pong";
  end;
  Thread.join t;
  ()

let test_no_starve =
  (* This shows that if we have external entities we need to be able to unblock
     a domain blocking on its unblocker. *)
  let open Affect_unix in
  Test.test' domain_count
    "Do not starve the scheduler by blocking in unblocker" @@
  fun domain_count ->
  let port = Port.make () in
  let forward = Port.make () in
  let t =
    Asyncable_thread.create @@ fun () ->
    Port.take forward;
    Unix.sleepf 0.0001;
    Port.offer port "ping";
  in
  begin Unix.main ?domain_count @@ fun () ->
    let next =
      let wait_one_year = Mtime.wait_for' Mtime.Span.(1 * year) "noping" in
      Action.choose [wait_one_year; Port.take' port]
    in
    Port.offer forward ();
    (* This synchronizes the work gets scheduled but if we can't interrupt
       the unblocker we end up waiting for a year before getting the ping. *)
    Test.string (Action.invoke next) "ping";
  end;
  Thread.join t;
  ()

let () = if !Sys.interactive then () else exit (Test_common.main ())
