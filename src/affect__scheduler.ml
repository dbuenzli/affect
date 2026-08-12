(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect__base
open Affect__action.Private
open Affect__async_fun.Private

let fun_nop () = ()

(* The scheduler is created by a so called *main domain* which is the
   domain that invokes the [main] function with a root asynchronous
   function [f] to run.

   This main domain creates local scheduling structures for each domain, spawns
   domains in order to meet the requested number of domains, setups
   their local action unblocker according to the unblocker setup
   protocol and schedules [f] for execution.

   Due to structured concurrency, when [f] returns, no other asynchronous
   activity can exist so the scheduler is stopped and spawned worker domains
   are joined. The implementation tries to make sure that any call to [main]
   eventually returns with all domains joined, even on internal errors
   (bugs) in which case [main] raises [Panic] after having joined them.

   Each domain of the scheduler has its own local scheduler with a local work
   queue. Note that since we use the [Work_stealing_queue_chase_lev]
   implementation, pushing work on this queue can only be done by (the main
   thread of) the domain itself. This is important when work is unblocked
   and scheduled by external entities (other schedulers, domains, threads).
   In this case we put this work in the special [external_queue] rather than
   trying to push it on a random local queue which would result in an illegal
   concurrent access to the queue.

   For scheduling each main thread of the domain of the scheduler does the
   following:

   0. If it's the main domain, try to find work in the [main_only] queue.
   1. Try to find work in the local queue.
   2. Try to find work in the global [external_queue].
   3. Try to steal work from the local queues of other domains.
   4. Try to unblock work by calling the local unblocker without blocking.
   5. If there are other domains that are still running, sleep the domain.
      If not, unblock work by calling the local unblocker with blocking.

   Note that for point 5. we make sure that a single domain ends up calling
   its local unblocker with blocking. *)

(* Work ready for execution *)

type ready = Async_fun.Call.t * (unit -> unit)
let ready_nil = Async_fun.Call.none, fun () -> assert false

(* Global scheduler *)

type epoch = int
type t =
  { epoch : epoch Atomic.t;
    (* [epoch] changes when something happens that should prevent a domain
       from sleeping or being blocked on its unblocker. Only change matters,
       this can overflow.  *)

    domains : domain_local iarray;
    (* The local schedulers of domains, positioned at their
       [Domain_local.index]. The main domain is at index [0]. *)

    only_main_queue : ready Synchronized_queue.t;
    (* FIFO queue for those calls that are only scheduled on the main domain. *)

    external_queue : ready Synchronized_queue.t;
    (* FIFO queue for those calls that are scheduled by action invocations
       unblocked by external computational activities (other schedulers,
       domains or threads). *)

    active_count : int Atomic.t;
    (* Number of domains which are in the Running state and not
       blocked on the unblocker. *)

    sleeping : domain_local Synchronized_stack.t;
    (* Stack which holds domains that are sleeping. It's only a necessary
       condition to be in that stack. An element of the stack may be running
       again without being removed. *)

    unblocker : Action.Unblocker.t;
    (* The primitive action invocations unblocker. *)

    blocked_on_unblocker : domain_local Atomic.t;
    (* Different from [Domain_local.none] if that domain has called its local
       unblocker in blocking mode. This scheduler maintains the invariant that
       there is only one of these domains. *)

    panic : string option Atomic.t;
    (* Stores panic messages in order to ensure orderly termination. *) }

(* Per domain local schedulers  *)

and domain_local_state =
| Running  (* Running a function, trying to schedule one or unblocking *)
| Sleeping (* Waiting on the [wakeup] condition variable to signal. *)
| Stopped  (* Stopped, sink state, tries to terminate the run loop asap. *)

and domain_local =
  { mutable id : Affect__trace.exec_id;
    (* Execution context identifier. Only mutated once when we start running
       the local scheduler to get the correct thread id. *)

    index : int; (* Index of the domain in the global scheduler array *)
    mutable call : Async_fun.Call.t;
    (* Executing asynchronous call. This is [Async_fun.none] if the domain
       is [Sleeping], [Stopped] or blocked on its local unblocker. *)

    random : Random.State.t; (* Random state for non-deterministic choices. *)
    ready_queue : ready Work_stealing_queue.t;
    (* Work ready for execution this is LIFO for the domain and FIFO for
       stealers. On yield a domain self-steals from that queue otherwise it
       gets back what it yielded. *)

    unblocker : Action.Unblocker.Domain_local.t; (* Local unblocker. *)
    mutable state : domain_local_state; (* Current domain state. *)
    state_mutex : Mutex.t; (* Mutex protecting [state]. *)
    wakeup : Condition.t; (* Condition variable used to wakeup the domain. *) }

let main_domain_index = 0
let domain_count s = Iarray.length s.domains
let get_main_domain s = Iarray.get s.domains main_domain_index
let get_local_domain s i = Iarray.get s.domains i
let epoch s = Atomic.get s.epoch
let change_epoch s = Atomic.incr s.epoch

module Domain_local = struct
  type t = domain_local

  let make ~main ~index ~unblocker () =
    let id = Affect__trace.{ sup = main; index; thread_id = -1} in
    let call = Async_fun.Call.none in
    let random = Random.State.make_self_init () in
    let ready_queue = Work_stealing_queue.make ~nil:ready_nil in
    let state = Running and state_mutex = Mutex.create () in
    let wakeup = Condition.create () in
    { id; index; call; random; ready_queue; unblocker; state; state_mutex;
      wakeup}

  let adjust_id dom =
    let id = { dom.id with thread_id = Affect__trace.current_thread_id () } in
    dom.id <- id

  (* Properties *)

  let id dom = dom.id
  let index dom = dom.index
  let is_main dom = dom.index = main_domain_index
  let ready_queue dom = dom.ready_queue
  let ready_queue_is_empty dom = Work_stealing_queue.is_empty dom.ready_queue
  let add_to_ready_queue r dom = Work_stealing_queue.push r dom.ready_queue
  let unblocker (dom : t) = dom.unblocker
  let is_stopped dom = dom.state = Stopped
  let call dom = dom.call
  let set_call dom call = dom.call <- call
  let set_call_none dom = dom.call <- Async_fun.Call.none

  (* Each main thread of the domain of a scheduler knows about its own
     local scheduler by storing it in local domain storage.

     When we try to retrieve it, we need to be careful about the context.
     Notably when we unblock actions since the unblock function can be
     executed in a totally different context. In this case given the
     scheduler [s] in which the action was performed, we need to make sure
     that the local domain scheduler in the DSL belongs to [s] and that
     it is the main thread of the domain that looks it up, see
     [get_or_external_exec_id].

     This could be a bit simplified a bit if we had thread local storage. *)

  let none =
    let id = Affect__trace.current_exec_id () in
    let unblocker = Action.Unblocker.Domain_local.none in
    make ~main:id.sup ~index:id.index ~unblocker ()

  let is_none dom = Repr.phys_equal dom none

  let key : t Domain.DLS.key = Domain.DLS.new_key (fun () -> none)
  let set dom = Domain.DLS.set key dom
  let tricky_get () =
    (* This is *tricky* because if you want to use the result without any
       check, you need to be sure that you are the main thread of the domain
       mentioned in the [id] of the result. *)
    Domain.DLS.get key

  let get_or_external_exec_id s =
    let dom = tricky_get () in
    if is_none dom then Either.Right (Affect__trace.current_exec_id ()) else
    if Int.equal (id dom).sup (id (get_main_domain s)).sup &&
       Int.equal (id dom).thread_id (Affect__trace.current_thread_id ())
    then Either.Left dom (* We are legit *)
    else Either.Right dom.id

  (* [trace] is exported in Fun.Async, but can be useful for debugging
     this module too. *)

  let trace fmt =
    let dom = tricky_get () in
    let current_thread = Affect__trace.current_thread_id () in
    let dom_id, call_id =
      if is_none dom || current_thread <> dom.id.thread_id
      then Affect__trace.current_exec_id (), Async_fun.Call.none_id
      else id dom, Async_fun.Call.id (call dom)
    in
    let report msg =
      Affect__trace.report (dom_id, (Async_fun_user (call_id, msg)))
    in
    Format.kasprintf report fmt

  let[@inline] trace_report dom t = Affect__trace.report (id dom, t)

  (* Operations *)

  let chaos dom = if Random.State.int dom.random 100 > 80 then assert false
  let random_int dom bound = Random.State.int dom.random bound
  let random_other_index ~domain_count dom =
    if domain_count <= 1 then 0 else
    let other = random_int dom (domain_count - 1) in
    if other < index dom then other else other + 1

  let unblocker_unblock dom ~block =
    trace_report dom (Domain_unblocker { block });
    let did_unblock =
      Action.Unblocker.Domain_local.unblock dom.unblocker ~block
    in
    trace_report dom Domain_resume;
    did_unblock

  let unblocker_set_block_bypass (dom : t) =
    Action.Unblocker.Domain_local.set_block_bypass dom.unblocker

  let stop s dom = Mutex.protect dom.state_mutex @@ fun () ->
    dom.state <- Stopped; change_epoch s; Condition.signal dom.wakeup;
    unblocker_set_block_bypass dom;
    ()

  let try_wakeup dom = Mutex.protect dom.state_mutex @@ fun () ->
    match dom.state with
    | Running | Stopped -> false
    | Sleeping -> Condition.signal dom.wakeup; true

  let sleep s dom ~epoch_seen = Mutex.protect dom.state_mutex @@ fun () ->
    if dom.state = Stopped then () else begin
      dom.state <- Sleeping;
      Synchronized_stack.push dom s.sleeping;
      trace_report dom Domain_sleep;
      while Int.equal (epoch s) epoch_seen
      do Condition.wait dom.wakeup dom.state_mutex done;
      trace_report dom Domain_resume;
      if dom.state <> Stopped then dom.state <- Running
    end
end

let trace = Domain_local.trace
let trace_report_exec_id id t = Affect__trace.report (id, t)
let trace_report = Domain_local.trace_report

let make ~main ~unblocker ~domain_count =
  let make_local_domain i =
    let unblocker = Action.Unblocker.domain_local unblocker i in
    Domain_local.make ~main ~index:i ~unblocker ()
  in
  let epoch = Atomic.make 0 in
  let domains = Iarray.init domain_count make_local_domain in
  let only_main_queue = Synchronized_queue.make () in
  let external_queue = Synchronized_queue.make () in
  let active_count = Atomic.make domain_count in
  let sleeping = Synchronized_stack.make () in
  let blocked_on_unblocker = Atomic.make Domain_local.none in
  let panic = Atomic.make None in
  { epoch; domains; only_main_queue; external_queue; active_count; sleeping;
    unblocker; blocked_on_unblocker; panic }

let stop s = Iarray.iter (Domain_local.stop s) s.domains
let add_to_only_main_queue r s = Synchronized_queue.add r s.only_main_queue
let add_to_external_queue r s = Synchronized_queue.add r s.external_queue
let has_panic s = Option.is_some (Atomic.get s.panic)
let add_panic s dom exn bt =
  let exn = Printexc.to_string exn in
  let bt = Printexc.raw_backtrace_to_string bt in
  let msg = Printf.sprintf "Exception: %s\n%s" exn bt in
  let add = function
  | None -> Some msg
  | Some prev -> Some (String.concat "\n" [prev; msg])
  in
  Atomic.update add s.panic;
  trace_report dom (Domain_panic msg)

(* Finding work *)

let try_steal_ready s dom = (* search in s.domains, starting at random *)
  let rec search ~self start i ~domain_count =
    let next = (i + 1) mod domain_count in
    let next = if next = self then (next + 1) mod domain_count else next in
    if i = self then search ~self start next ~domain_count else
    let q = Domain_local.ready_queue (get_local_domain s i) in
    match Work_stealing_queue.steal q with
    | Some _ as ready -> ready
    | None when next = start -> None
    | None -> search ~self start next ~domain_count
  in
  let domain_count = domain_count s in
  if domain_count = 1 then None else
  let self = Domain_local.index dom in
  let start = Domain_local.random_other_index ~domain_count dom in
  search ~self start start ~domain_count

let try_only_main_ready s = Synchronized_queue.take s.only_main_queue
let try_external_ready s = Synchronized_queue.take s.external_queue
let try_local_ready ~self_steal dom =
  let q = Domain_local.ready_queue dom in
  if self_steal
  then Work_stealing_queue.steal q (* for yields to avoid LIFO *)
  else Work_stealing_queue.pop q

let find_ready_for_worker ~self_steal s dom =
  match try_local_ready ~self_steal dom with
  | Some _ as ready -> ready
  | None ->
      match try_external_ready s with
      | Some _ as ready -> ready
      | None -> try_steal_ready s dom

let find_ready_for_main ~self_steal s dom = match try_only_main_ready s with
| Some _ as ready -> ready
| None -> find_ready_for_worker ~self_steal s dom

let find_ready ~self_steal s dom =
  if Domain_local.is_main dom
  then find_ready_for_main ~self_steal s dom
  else find_ready_for_worker ~self_steal s dom

(* Scheduling work ready to continue *)

let try_unblock_blocked_on_unblocker s =
  let dom = Atomic.get s.blocked_on_unblocker in
  if Domain_local.is_none dom then false else
  (Domain_local.unblocker_set_block_bypass dom; true)

let wakeup_main s = ignore (Domain_local.try_wakeup (get_main_domain s))
let rec try_wakeup_one s = match Synchronized_stack.pop s.sleeping with
| Some dom -> if Domain_local.try_wakeup dom then () else try_wakeup_one s
| None -> ()

let schedule_ready s dom (call, _ as ready) =
  let is_main_schedule = Async_fun.Call.is_only_main call in
  let () =
    if is_main_schedule then add_to_only_main_queue ready s else
    if Domain_local.is_none dom then add_to_external_queue ready s else
    Domain_local.add_to_ready_queue ready dom
  in
  change_epoch s;
  (* Wake up one thing, for main we may wakeup two but it's harmless.
     We just don't want things to become busy again with a blocked unblocker. *)
  if try_unblock_blocked_on_unblocker s
  then (if is_main_schedule then wakeup_main s else ())
  else (if is_main_schedule then wakeup_main s else try_wakeup_one s)

(* Running work ready to continue *)

let continue_ready dom (call, continue as ready) =
  (* If this assertion fails it usually means that the work stealing queue
     implementation in [Affect_base] is [Work_stealing_queue_chase_lev] and
     that there were concurrent push/pop operations which is NEIN! *)
  assert (ready != ready_nil);
  trace_report dom (Async_fun_continue (Async_fun.Call.id call));
  Domain_local.set_call dom call;
  continue ()

let unblock_or_sleep ~epoch_seen s dom =
  if Domain_local.unblocker_unblock dom ~block:false then () else
  if Atomic.fetch_and_add s.active_count (-1) = 1 &&
     Atomic.compare_and_set s.blocked_on_unblocker Domain_local.none dom
  then begin
    if Int.equal (epoch s) epoch_seen
    then ignore (Domain_local.unblocker_unblock dom ~block:true);
    Atomic.set s.blocked_on_unblocker Domain_local.none;
    Atomic.incr s.active_count;
  end else begin
    Domain_local.sleep s dom ~epoch_seen;
    Atomic.incr s.active_count
  end

let rec run_next_ready s dom =
  if Domain_local.is_stopped dom then () else
  let epoch_seen = epoch s in
  match find_ready ~self_steal:false s dom with
  | Some (call, continue as ready) -> continue_ready dom ready
  | None -> unblock_or_sleep ~epoch_seen s dom; run_next_ready s dom

(* Run domains *)

let do_yield s dom k =
  let call = Domain_local.call dom in
  let ready = call, fun () -> Effect.Deep.continue k () in
  Domain_local.set_call_none dom;
  trace_report dom (Async_fun_yield (Async_fun.Call.id call));
  schedule_ready s dom ready;
  match find_ready ~self_steal:true (* don't pop the yield *) s dom with
  | Some ready -> continue_ready dom ready
  | None -> run_next_ready s dom

let do_async_call s dom ~handler ~schedule run f =
  let notify_finish af =
    let call_id = Async_fun.id_int af in
    let dom = Domain_local.tricky_get () in
    trace_report dom (Async_fun_return call_id);
  in
  let parent = Domain_local.call dom in
  let af = Async_fun.make ~notify_finish ~handler ~schedule ~parent () in
  let run_f () = run s (fun () -> Async_fun.run af f) in
  let parent = Async_fun.Call.id parent and id = Async_fun.id_int af in
  trace_report dom (Async_fun_call { parent; id });
  schedule_ready s dom (Async_fun.Call.of_async_fun af, run_f);
  af

let do_action_invoke s dom action k =
  let unblock = Async_fun.unblock_action_invoke (Domain_local.call dom) in
  match Action.Invocation.make ?unblock action with
  | Either.Left (exn, bt) -> Effect.Deep.discontinue_with_backtrace k exn bt
  | Either.Right invocation ->
      let continue = Action.Result.deep_continue_thunk k in
      match Action.Invocation.do_poll invocation ~continue with
      | Some continue -> continue ()
      | None ->
          let call = Domain_local.call dom in
          let call_id = Async_fun.Call.id call in
          let meta = Action.Invocation.meta invocation in
          let unblock result =
            let exec_id, dom = match Domain_local.get_or_external_exec_id s with
            | Left dom -> Domain_local.id dom, dom
            | Right exec_id -> exec_id, Domain_local.none (* external queue *)
            in
            trace_report_exec_id exec_id (Action_unblock (call_id, meta));
            schedule_ready s dom (call, continue result)
          in
          Domain_local.set_call_none dom;
          trace_report dom (Action_block (call_id, meta));
          Action.Invocation.do_block ~unblock invocation;
          run_next_ready s dom

let do_action_invoke_poll s dom action k =
  let unblock = Async_fun.unblock_action_invoke (Domain_local.call dom) in
  Action.do_invoke_poll ?unblock action k

let do_sys_break s dom =
  let call = Domain_local.call dom in
  Domain_local.set_call_none dom;
  Async_fun.do_sys_break call;
  run_next_ready s dom

let rec run s f = match f () with
| () ->
    let dom = Domain_local.tricky_get () in
    Domain_local.set_call_none dom;
    run_next_ready s dom
| effect Async_fun.Call (handler, schedule, f), k ->
    Effect.Deep.continue k @@
    do_async_call s (Domain_local.tricky_get ()) ~handler ~schedule run f
| effect Action.Invoke action, k ->
    do_action_invoke s (Domain_local.tricky_get ()) action k
| effect Action.Invoke_poll action, k ->
    do_action_invoke_poll s (Domain_local.tricky_get ()) action k
| effect Async_fun.Yield, k ->
    do_yield s (Domain_local.tricky_get ()) k
| effect Async_fun.Get_parallel_count, k ->
    Effect.Deep.continue k (domain_count s)
| effect Async_fun.Get_current_call, k ->
    Effect.Deep.continue k (Domain_local.call (Domain_local.tricky_get ()))
| exception Sys.Break ->
    do_sys_break s (Domain_local.tricky_get ())

let run_domain s dom ~init =
  try
    Domain_local.adjust_id dom;
    Domain_local.set dom;
    trace_report dom Domain_start;
    Action.Unblocker.domain_install s.unblocker (Domain_local.unblocker dom);
    init (); run s fun_nop;
    assert (Domain_local.is_stopped dom);
    (if not (has_panic s) then assert (Domain_local.ready_queue_is_empty dom));
    Action.Unblocker.domain_uninstall s.unblocker (Domain_local.unblocker dom);
    Domain_local.set Domain_local.none;
    trace_report dom Domain_stop
  with exn ->
    let bt = Printexc.get_raw_backtrace () in
    if Exn.is_runtime_system exn then Exn.trap exn bt;
    add_panic s dom exn bt;
    stop s

let run_worker_domain s i = run_domain s (get_local_domain s i) ~init:fun_nop
let run_main_domain
    s ?(schedule = Async_fun.Schedule.default)
    ?(handler = Async_fun.Call_handler.none) f
  =
  let dom = get_main_domain s in
  let notify_finish af =
    (* [Fun.Async.get]s on a function are unblocked only after [notify_finish]
       so continuations could be invoked after this call but there should be
       none for this scheduler: children must not wait for their parent
       otherwise there is a deadlock. So at this point the local domain queues
       must be empty and will remain so. As such it's ok to [stop] here, in
       fact unless a panic occured (see [run_domain]), this is the only place
       where the scheduler is stopped. *)
    let call_id = Async_fun.id_int af in
    trace_report (Domain_local.tricky_get ())  (Async_fun_return call_id);
    assert (Synchronized_queue.is_empty s.only_main_queue);
    stop s
  in
  let parent = Async_fun.Call.none in
  let af = Async_fun.make ~notify_finish ~handler ~schedule ~parent () in
  let schedule_root () =
    let run_f () = run s (fun () -> Async_fun.run af f) in
    let parent = Async_fun.Call.id parent and id = Async_fun.id_int af in
    trace_report dom (Async_fun_call { parent; id });
    schedule_ready s dom (Async_fun.Call.of_async_fun af, run_f);
  in
  run_domain s dom ~init:schedule_root;
  Async_fun.result af

(* Main function *)

let spawn_workers ~domain_spawn s =
  let spawn_worker i = domain_spawn @@ fun () -> run_worker_domain s (i + 1) in
  Iarray.init (domain_count s - 1) spawn_worker

let main
    ?(unblocker = Action.Unblocker.nothing ())
    ?(domain_spawn = Domain.spawn)
    ?(domain_count = Domain.recommended_domain_count ())
    ?schedule ?handler func
  =
  let () = Action.Unblocker.init unblocker ~domain_count in
  let s = make ~main:(Domain.self () :> int) ~unblocker ~domain_count in
  let workers = spawn_workers ~domain_spawn s in
  let finally () = Iarray.iter Domain.join workers in
  let result =
    Fun.protect ~finally @@ fun () -> run_main_domain s ?schedule ?handler func
  in
  let () = try Action.Unblocker.deinit unblocker () with
  | exn when not (Exn.is_runtime_system exn) ->
      let bt = Printexc.get_raw_backtrace () in
      add_panic s (get_main_domain s) exn bt
  in
  match Atomic.get s.panic with
  | Some panic -> raise (Panic.Panic panic)
  | None ->
      match result with
      | Some result -> Action.Result.return result
      | None -> assert false
