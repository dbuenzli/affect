(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Asynchronous functions

    @canonical Affect.Fun.Async *)

open Affect__action.Private

(** Call handlers.

    A call handler wraps an asynchronous function call [f] before it starts
    executing. It is responsible for making the [f ()] call. The handler
    is also used to make all the asynchronous calls done by [f] itself and
    its descendents.

    The composition order of handlers matches the syntactic scope: outer scopes
    are called after inner ones. More precisely in the example below [f] is
    called with [(]{!Call_handler.compose}[ h0 h1).handle f]:

    {[
    let h0 = Fun.Async.Call_Handler.{ handle = … }
    let h1 = Fun.Async.Call_Handler.{ handle = … }
    let ret =
      Fun.Async.call ~handler:h0 @@ fun () ->
      Fun.Async.call ~handler:h1 @@
      f (* Formally: h0.handle (fun () -> h1.handle f) *)
    ]}

    Handlers can be used to make sure effects are handled in
    asynchronous function calls according to expected syntactic scopes,
    see {{!page-cookbook.basics_effects}the cookbook}.

    @canonical Affect.Fun.Async.Call_handler *)
module Call_handler : sig
  type t = { handle : 'a. (unit -> 'a) -> 'a }
  (** The type for handlers.

      Given a function [f], [handle] must call [f ()] and returns its value
      or exception.

      {b Warning.} [handle] must be synchronization safe. It
      can be executed in parallel and/or on multiple different domains
      over time. *)

  val call : t -> (unit -> 'a) -> 'a
  (** [call h f] is [h.handle f]. *)

  val none : t
  (** [none] is a handler that just executes the function. *)

  val compose : t -> t -> t
  (** [(compose h0 h1).handle f] is [h0.handle (fun () -> h1.handle f)]. *)

  val of_list : t list -> t
  (** [of_list hs] is [List.fold_right compose hs f none]. *)
end

(** Scheduling parameters.

    @canonical Affect.Fun.Async.Schedule *)
module Schedule : sig

  (** {1:priorities Priorities}

      {b Note.} Priorities are currently ignored by
      {!Fun.Async.main}. *)

  type priority =
  | Low (** Background computation. *)
  | Normal (** Normal computation. *)
  | High (** Urgent or interactive computation. *)
  (** The type for scheduling priority hints. *)

  val pp_priority : Format.formatter -> priority -> unit
  (** [pp_priority] formats priorities for inspection. *)

  (** {1:params Scheduling parameters} *)

  type t
  (** The type for function scheduling parameters. *)

  val make : ?only_main:bool -> ?priority:priority -> unit -> t
  (** [make ()] are scheduling parameters with:
      {ul
      {- [only_main] if [true] the function is always only scheduled
         the main domain of the scheduler, i.e. the one which executes
         {!Affect.Fun.Async.main}. Defaults to [false].}
      {- [priority] is a scheduling priority hint. Defaults to the caller's
         priority.}} *)

  val only_main : t -> bool
  (** See {!make}. *)

  val priority : t -> priority option
  (** See {!make}. [None] means that the priority is inherited from the
      caller. *)

  val default : t
  (** [default] is {!make}[ ()]. *)
end

type !'a t

val call :
  ?handler:Call_handler.t -> ?schedule:Schedule.t -> (unit -> 'a) -> 'a t

val call_trap_exn :
  ?handler:Call_handler.t -> ?schedule:Schedule.t -> (unit -> unit) -> unit

val trap_exn : exn -> unit
val from_val : 'a -> 'a t
val from_exn : exn -> Printexc.raw_backtrace -> 'a t
val yield : unit -> unit

(* Getting results *)

val get : 'a t -> 'a
val get_all : 'a t list -> 'a list
val get_either : 'a t -> 'b t -> ('a, 'b) Either.t
val get' : 'a t -> 'a Action.t

(* Cancelling *)

exception Cancelled
val cancel : 'a t -> unit
val cancel_current : unit -> unit
val is_cancelled : 'a t -> bool
val is_current_cancelled : unit -> bool
val check_cancellation : unit -> unit
val mask_cancellation : (unit -> 'a) -> 'a
val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
val wait_cancelled : 'a t -> 'b -> 'b Action.t

(* Properties *)

val id : 'a t -> 'a Type.Id.t
val schedule : 'a t -> Schedule.t
val priority : 'a t -> Schedule.priority
val has_returned : 'a t -> bool

(* Sizing work *)

val parallel_count : unit -> int
val parallel_count_override : int -> Call_handler.t
val parallel_worker_count : unit -> int
val divide_work : size:int -> worker_count:int -> int * (int -> int * int)

(* Formatting *)

val pp : Format.formatter -> 'a t -> unit

(** Existential {!Fun.Async.t}.

    @canonical Affect.Fun.Async.Call *)
module Call : sig

  (** {1:call_ids Call identifiers} *)

  type id = int
  (** The type for call identifiers. *)

  val none_id : id
  (** [none_id] is [id none]. *)

  val pp_id : Format.formatter -> id -> unit
  (** [pp_id] formats call ids. *)

  (** {1:calls Calls} *)

  type 'a async_fun := 'a t

  type t
  (** The type for existential {!Affect.Fun.Async.t} values. *)

  val current : unit -> t
  (** [current ()] is the call handle of the caller. *)

  val none : t
  (** [none] is a call handle for when there is none. For example it is
      the {!parent}  of the root asynchronous call. *)

  (** {1:props Properties} *)

  val id : t -> id
  (** [id call] is a unique identifier for the call. It matches the value
      {!Type.Id.uid} of the {!Affect.Fun.Async.id} of the asynchronous
      function call. *)

  val parent : t -> t
  (** [parent c] is the parent of [c] that is the function that called it.
      If [c] is the root asynchronous function this is {!none}. *)

  val schedule : t -> Schedule.t
  (** [schedule c] is the schedule of [c] specified at {!Fun.Async.val-call}
      time. *)

  val priority : t -> Schedule.priority
  (** [priority c] is the current priority of [c]. *)

  (** {1:cancellation Cancellation} *)

  val cancel : t -> unit
  (** [cancel c] is {!Affect.Fun.Async.cancel} on the call. *)

  val is_cancelled : t -> bool
  (** [is_cancelled c] {!Affect.Fun.Async.is_cancelled} on the call. *)

  val wait_cancelled : t -> 'tag -> 'tag Action.t
  (** [wait_cancelled c tag] is {!Affect.Fun.Async.wait_cancelled} on the
      call. *)

  (** {1:preds Predicates and comparisons} *)

  val is_none : t -> bool
  (** [is_none c] is [true] if and only if [c] is {!none}. *)

  val is_only_main : t -> bool
  (** [is_only_main c] is [true] if and only if [c] is only scheduled on the
      main domain of the scheduler. *)

  val has_returned : t -> bool
  (** [has_returned c] is {!Affect.Fun.Async.has_returned} on the call. *)

  val equal : t -> t -> bool
  (** [equal c0 c1] is [true] if and only if [c0] and [c1] {{!val-id}identify}
      as the same call. *)

  val compare : t -> t -> int
  (** [compare] is a total order on calls compatible with {!equal}. *)

  (** {1:conv Converting} *)

  val of_async_fun : 'a async_fun -> t
  (** [of_async_fun f] is [f] as a call. *)

  val to_async_fun : 'a Type.Id.t -> t -> 'a async_fun option
  (** [to_async_fun tid c] is [Some f] with [f] the asynchronous function
      of [c] if and only if [tid] is the {!Fun.Async.id} of [f]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats calls like {!Fun.Async.pp}. *)
end

(** Private definitions (unstable).

    @canonical Affect.Fun.Async.Private *)
module Private : sig

  (** Asynchronous function internals and effects.

      These definitions are subject to change even between minor versions of
      the library.

      {b Note.} You should never use these definitions or handle these effects
      in the scope of a function given to {!Affect.Fun.Async.main}. These
      definitions are exposed to allow {!Affect.Fun.Async} asynchronous function
      calls to be handled by other schedulers. See an example
      {{:https://github.com/dbuenzli/affect/blob/master/test/test_thread.ml}
      here}. *)
  module Async_fun : sig
    module Call_handler = Call_handler
    module Schedule = Schedule
    module Call = Call

    val make :
      ?notify_finish:('a t -> unit) -> handler:Call_handler.t ->
      schedule:Schedule.t -> parent:Call.t -> unit -> 'a t
    (** [make handler ~schedule ~parent ()] is an asynchronous function call
        with given properties. This takes care of composing the call
        handler of [parent] with [handler] and register the call in
        parent's [children]. For the root asynchronous function use
        {!Call.none} for the parent.

        [notify_finish] is called once with the created call when the
        asynchronous function's result has been computed and all its
        asynchronous calls have returned just before the parent or
        entities blocked on [get]ing the function result get unblocked.

        Note that this is just data about the call, it doesn't carry the
        actual computation. It provides however the location where the
        function result is stored. *)

    val run : 'a t -> (unit -> 'a) -> unit
    (** [run af f] runs the asynchronous function [af] with the body [f]
        wrapped in the call handler of [af]. Takes care of structured
        concurrency and calling the [notfiy_finish] given in {!make}
        when it's time to do so. *)

    val id_int : 'a t -> int
    (** [id_int af] is [Type.Id.uid (id af)]. *)

    val set_priority : 'a t -> Schedule.priority -> unit
    (** [set_priority af p] sets the priority of [af] to [p]. *)

    val result : 'a t -> 'a Action.Result.t option
    (** [result af] is the result of [af]. This is guaranteed to be [Some _]
        after [notify_finish] given to {!make} has been called. Normally
        this is only needed to get the result of the root asynchronous
        function. *)

    val do_sys_break : Call.t -> unit
    (** [do_sys_break c] can be used by a scheduler if it receives a
        {!Sys.break} while running [c]. At that point you no longer
        have a body to run for [c] so the function cancels [c] and
        moves it state to return a {!Cancelled} exception if it's still
        running or waiting to return. *)

    val unblock_action_invoke : Call.t -> 'b Action.Primitive.t option
    (** [unblock_action_invoke call] is the primitive action (if any) that
        needs to be given to {!Action.Private.Action.Invocation.make} when you
        handle an {!Action.Private.Action.Invoke} effect for [call]. It has
        the whole logic (including masking) to handle the cancellation model of
        asynchronous functions. *)

    (** {1:effects Effects} *)

    type 'a Stdlib.Effect.t +=
    | Call : Call_handler.t * Schedule.t * (unit -> 'a) -> 'a t Effect.t
    (** Performed on {!Affect.Fun.Async.val-call} *)
    | Yield : unit Effect.t
    (** Performed on {!Affect.Fun.Async.yield} *)
    | Get_current_call : Call.t Effect.t
    (** Performed on {!Affect.Fun.Async.Call.current} *)
    | Get_parallel_count : int Effect.t
    (** Performed on {!Affect.Fun.Async.parallel_count} *)
    (** Effects for handling asynchronous functions.

        These effects assume that your scheduler is maintaining
        the currently executing asynchronous function on the current
        processor in a [current] variable. Given that here is how you must
        handle these effects:

        {ul
        {- On {!extension-Call}[ (handle, schedule, f)] you must must create
           an asynchronous call [af] with [make ~handle ~schedule
           ~parent:current] and arrange for [f] to be {b scheduled} for
           execution in parallel or concurrently (with [current] set as
           {!Call.of_async_fun}[ af]) and immediately continue with [af].
           [current] must not change, the caller {b must not} be suspended.
        }
        {- On {!Yield}, unless no other call is ready for execution,
           you must arrange for [current] to be scheduled later and execute
           another call ready for execution as [current].}
        {- On {!Get_current_call} you must immediately continue with
           [current].}
        {- On {!Get_parallel_count} you must immediately continue
           with a {b strictly positive} parallel count you find suitable to
           answer to a call to {!Fun.Async.parallel_count}. Note that the
           handling of this effect is user-overidable from the API with
           {!Fun.Async.parallel_count_override}.}
        {- On {!Sys.Break} you should call {!do_sys_break} on
           [current] and execute another call ready for execution
           as [current]. Unless you think there's something smarter to do.}}

        {b Important.} Additionaly when you handle
        {{!Action.Private.Action.effects}the effects} for action invocations
        for [current] you must use the value of
        {!unblock_action_invoke}[ current]
        and pass it as the [unblock] argument of the
        {!Action.Private.Action.Invocation.make} you create. This makes
        sure that the asynchronous function cancellation model is handled
        correctly. *)
  end
end
