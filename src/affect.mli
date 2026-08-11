(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Asynchronous functions. Open to use it. *)

module Action = Affect__action

(** Extended {!Stdlib.Fun} module. *)
module Fun : sig

  include module type of Stdlib.Fun (** @closed *)

  (** {1:asynchronous Asynchronous functions} *)

  (** Asynchronous functions.

      Asynchronous functions run in parallel or concurrently to the
      caller. The caller does not return before the asynchronous functions
      it called do.

      Read the short {{!concurrency_model}concurrency model}. *)
  module Async : sig

    (** {1:asynchronous Asynchronous functions} *)

    module Call_handler = Affect__async_fun.Call_handler
    module Schedule = Affect__async_fun.Schedule

    (* In the future we could likely move the comments here to
       Affect__async_fun and include that module type with @inline but
       the brzo doc (or odoc?) renderer seems to lose the comments. *)

    type 'a t
    (** The type for a handle on an asynchronous function call returning a
        value of type ['a] or an exception. *)

    val call :
      ?handler:Call_handler.t -> ?schedule:Schedule.t -> (unit -> 'a) -> 'a t
    (** [call f] executes [f ()] in parallel or concurrently to the caller and
        immediately returns a call handle for it.
        {ul
        {- If [handler] is provided [call f] executes {!Call_handler.call}[
           handler f] and all asynchronous calls in [f] also get wrapped
           the handler.}
        {- [schedule] are scheduling parameters. Defaults to
           {!Schedule.default}.}} *)

    val call_trap_exn :
      ?handler:Call_handler.t -> ?schedule:Schedule.t ->
      (unit -> unit) -> unit
    (** [call_trap_exn] is like {!val-call} except it traps unhandled
        exceptions with {!trap_exn}. The call handle is not returned,
        the function is assumed to be synchronized by structured
        concurrency. If you would like to have the call handle use {!trap_exn}
        directly in the function you {!call}.

        See {{!page-cookbook.basics_exn}the cookbook}. *)

    val trap_exn : exn -> unit
    (** [trap_exn exn] traps [exn] by giving it to
        {!Printexc.default_uncaught_exception_handler} with
        the following exceptions:
        {ul
        {- {!Cancelled} is silently discarded}
        {- The runtime system asynchronous exception are not trapped
           they are re-raised by the function.}} *)

    val from_val : 'a -> 'a t
    (** [from_val] is [call (Fun.const v)] but eschews scheduling. *)

    val from_exn : exn -> Printexc.raw_backtrace -> 'a t
    (** [from_exn exn bt] is
        [call (fun () -> Printexc.raise_with_backtrace exn bt)] but eschews
        scheduling. *)

    val yield : unit -> unit
    (** [yield ()] cooperatively suspends the caller. *)

    (** {1:getting Getting results} *)

    val get : 'a t -> 'a
    (** [get f] blocks until the call [f] returns and continues with its
        return value or exception. *)

    val get_all : 'a t list -> 'a list
    (** [get_all fs] is [List.map get fs]. *)

    val get_either : 'a t -> 'b t -> ('a, 'b) Either.t
    (** [get_either f0 f1] invokes the action {!Action.either}[ (get' f0)
        (get' f1)]. *)

    (** {2:get_actions Actions} *)

    val get' : 'a t -> 'a Action.t
    (** [get' f] is the action for {!get}. An action invocation is permanently
        enabled and synchronize with [f]'s return value or exception
        once the function has returned. *)

    (** {1:cancellation Cancellation} *)

    exception Cancelled
    (** The exception thrown by {!Action.invoke} or {!check_cancellation} to
        indicate that the current asynchronous function is cancelled. *)

    (** {2:cancelling Cancelling} *)

    val cancel : 'a t -> unit
    (** [cancel f] marks [f] as being cancelled. If [f] already returned this
        has no effect. Otherwise all asynchronous functions still
        active in [f] also get marked as cancelled and all future
        ones. Functions blocked on an {!Action.invoke} are unblocked
        by raising {!Cancelled} and so do all future action invocations
        unless invoked in a {!mask_cancellation} scope. *)

    val cancel_current : unit -> unit
    (** [cancel_current ()] marks the caller as cancelled. All current
        and future asynchronous functions of the caller are marked as
        cancelled and all future action invocations raise {!Cancelled}
        unless they are invoked in a {!mask_cancellation} scope. *)

    (** {2:checking Checking} *)

    val is_cancelled : 'a t -> bool
    (** [is_cancelled f] is [true] if and only if [f] is marked as cancelled. *)

    val is_current_cancelled : unit -> bool
    (** [is_current_cancelled] is [true] if and only if caller is marked
        as cancelled. *)

    val check_cancellation : unit -> unit
    (** [check_cancellation ()] raises {!Cancelled} if and only if the
        caller is marked as cancelled. *)

    (** {2:masking Masking} *)

    val mask_cancellation : (unit -> 'a) -> 'a
    (** [mask_cancellation f] executes [f ()]. During the call, regardless of
        the cancellation mark of the caller:
        {ul
        {- Calls to {!Action.invoke} do not raise {!Cancelled}
           due to the cancellation mark of the caller. Note that
           they may still do so for other reasons, for example if you
           {!get} the result of a function that raises that exception.}}
        The masking {b does only that} and notably the following is left
        unchanged:
        {ul
        {- If you call asynchronous functions in [f] those inherit the
           actual cancellation mark of the caller.}
        {- {!is_current_cancelled} still returns the actual
           cancellation mark of the caller.}
        {- {!check_cancellation} still raises according to the
           actual cancellation mark of the caller.}
        {- {!cancel_current} still ensures that the caller gets
           marked as cancelled.}} *)

    val protect : finally:(unit -> unit) -> (unit -> 'a) -> 'a
    (** [protect] is like {!Fun.protect} except [finally] is called in a
        {!mask_cancellation} scope. *)

    (** {2:cancellation_actions Actions} *)

    val wait_cancelled : 'a t -> 'tag -> 'tag Action.t
    (** [wait_cancelled f tag] is an action for synchronizing on the
        cancellation of [f]. An action invocation is permanently
        enabled and synchronize with [tag] once [f] gets marked as
        cancelled (if ever). *)

    (** {1:properties Properties} *)

    val id : 'a t -> 'a Type.Id.t
    (** [id f] is the unique identifier of [f]. *)

    val schedule : 'a t -> Schedule.t
    (** [schedule f] is the initial schedule of [f].  *)

    val priority : 'a t -> Schedule.priority
    (** [priority f] is the current priority of [f].  *)

    val has_returned : 'a t -> bool
    (** [has_returned f] is [true] iff [f] has computed a value or an
        exception and that all its children have too, a {!get} won't
        block.

        {b Warning.} Do not use this to perform busy waiting or reason on
        scheduling. While a {!get} won't block it doesn't mean that those
        waiting on [f] have been unblocked yet or that the parent is aware
        of [f] having returned. *)

    (** {1:calls Calls} *)

    module Call = Affect__async_fun.Call

    (** {1:dividing_parallel_work Dividing parallel work} *)

    val parallel_count : unit -> int
    (** [parallel_count ()] is a strictly positive amount of parallel
        processors available for performing parallel work at the time
        of call. It can be used to size work items, see the
        {{!page-cookbook.basics_work_items}cookbook}. See also
        {!parallel_worker_count}. *)

    val parallel_count_override : int -> Call_handler.t
    (** [parallel_count_override count] is a call handler so that
        {!parallel_count}[ ()] is [count] in handled calls.
        This can be by applications to control library computation that use
        {!parallel_count} to divide their work. Library functions should
        not use this function.

        @raise Invalid_argument if [count] is not strictly positive. *)

    val parallel_worker_count : unit -> int
    (** [parallel_worker_count ()] is
        [Int.max 1 (]{!parallel_count}[ () - 1)]. *)

    val divide_work : size:int -> worker_count:int -> int * (int -> int * int)
    (** [divide_work ~size ~worker_count] divides [size] elements among
        [worker_count] workers as evenly as possible. It uses integer
        division and distributes any remainder on the first workers,
        see the {{!page-cookbook.basics_work_items}cookbook}. The function
        returns a pair [(workers_used, range)] with
        {ul
        {- [workers_used] the number of workers used among [worker_count].
           Can be smaller than [worker_count] if there is not enough work
           or [0] if [size] is [0].}
        {- [range] a function which given an index [w] in
           \[[0];[workers_used-1]\] returns an inclusive subrange
           [(first,last)] of the range \[[0];[size-1]\] of elements to process
           by the worker indexed by [w].
           The [range] function raises [Invalid_argument] if [w] is out of
           bounds and in particular always does if [workers_used] is [0].}}

        @raise Invalid_argument if [worker_count < 1] or if [size < 0]. *)

    (** {1:running Running} *)

    exception Panic of string
    (** This exception is raised by {!main} if there in an internal error in
        the scheduler or misbehaving action primitives. It is not
        meant to be handled, let it flow at the toplevel to print it
        and abort your program or restart a scheduler.  *)

    val main :
      ?unblocker:Action.unblocker ->
      ?domain_spawn:((unit -> unit) -> unit Domain.t) -> ?domain_count:int ->
      ?schedule:Schedule.t -> ?handler:Call_handler.t ->
      (unit -> 'a) -> 'a
    (** [main f] creates a root asynchronous function call for [f] and runs it
        to completion. The function either returns with [f]'s value or
        exception or it raises {!Panic}. In all case, when the
        function returns the spawned domains are properly joined.

        The optional parameters are:

        {ul
        {- [handler], the call handler for [f]. All descendent asynchronous
           calls inherit it. See {!Affect.Fun.Async.val-call}.}
        {- [schedule], the scheduling parameters for [f].
           See {!Affect.Fun.Async.val-call}.}
        {- [domain_count], the total number of domains including
           the domain that execute [main]. Defaults to
           {!Domain.recommended_domain_count}.}
        {- [domain_spawn], function used to spawn the worker domains.
           Defaults to {!Domain.spawn}. It can be used for example
           to devise a spawn that blocks signal delivery in worker
           domains.}
        {- [unblocker], the unblocker used to unblock external
           primitive actions. Nothing gets unblocked by default.
           See for example {!Affect_unix.Unix.unblocker}}}

        We call the {e main domain} the domain that calls [main].
        This can be different from what the OCaml runtime
        main domain as determined by {!Domain.is_main_domain}.

        Invoking {!main} anywhere in [f] is not recommended, except
        for mind twisters.

        {b Multi-scheduling, other domains and threads.}
        It is possible to use {!main} in different domains if you want to have
        isolated computing realms. It also possible to communicate between
        them using actions, same holds for another domain or thread as long
        as it knows how to handle actions.

        However for now you should not try to use
        actions to communicate bewteen the two realms or to communicate between
        a [main] and a thread or a domain. This limitation will be lifted
        in the future, see the {{:https://github.com/dbuenzli/affect/issues}
        issue tracker}.

        {b OCaml runtime asynchronous exceptions.} OCaml runtime
        asynchronous exceptions are left to flow to the top level of the
        domain scheduler loops. At that point for {!Sys.Break} we cancel
        the currently executing asynchronous call, set its result to raise
        {!Cancelled} and try to continue the scheduler – unclear if it
        that is a good strategy, but we cannot finish the body due to lack of
        continuation. For the others we trap them and we try to
        panic the scheduler but that may not work e.g. on [Out_of_memory].

        {b Note.} The current scheduler does not implement priorities.

        @raise Invalid_argument If [domain_count] is smaller than 1.

        @raise Panic If an internal scheduler error occurs and [f] cannot be
        run to completion.  *)

    (** {1:tracing Tracing} *)

    val pp : Format.formatter -> 'a t -> unit
    (** [pp] formats asynchronous function identifiers. *)

    val trace : ('a, Format.formatter, unit, unit) format4 -> 'a
    (** [trace fmt …] formats a message and reports it as
        {!Trace.Async_fun_user} trace attributed to the caller (or
        {!Call.none} if this is not running under a {!main}). *)

    module Trace = Affect__trace

    (** {1:private Private} *)

    module Private = Affect__async_fun.Private
  end
end

module Port = Affect__port
module Cell = Affect__cell
module Semaphore = Affect__semaphore
