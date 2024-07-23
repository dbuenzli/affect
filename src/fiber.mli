(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Fibers.

    Fibers are non-parallel and cooperatively scheduled threads of
    executions. In a domain no two fibers ever execute at the same
    time.

    A fiber can {{!val-spawn}spawn} concurrent fibers, called {e
    spawns}, each executing their own function. All the spawns of a
    fiber have to terminate before it can terminate itself.

    A fiber {e terminates} either by returning a value or after it
    was aborted:

    {ul
    {- When a fiber's function returns a value, it waits for each of
       its spawns to terminate before {{!ret}returning} its value.}
    {- When a fiber is aborted, it first aborts all its spawns
       that are not yet terminated, then gets aborted itself and terminates
       without a value.}}

    Fibers get {e aborted} by raising the {!Abort} exception in their
    function. You should only ever handle that exception to re-raise
    it after a cleanup. In fact it's better to never handle it and
    simply use {!Fun.protect} and the [finally] argument of fiber
    spawns for setting up your cleanup functions.

    A fiber can be aborted explicitely via {!val-abort} or {!self_abort}
    or implicitly in case an uncaught exception occurs in the fiber
    function. In the latter case the exception is trapped via
    {!Printexc.default_exception_handler} (no API to access the actual
    handler at the moment) and the fiber is aborted. *)

(** {1:fibers Fibers} *)

type 'a t
(** The type for fibers returning values of type ['a]. *)

val yield : unit -> unit
(** [yield ()] cooperatively yields the calling fiber. Other fibers get a
    chance to execute at that point. *)

val spawn : ?finally:(unit -> unit) -> (unit -> 'a) -> 'a t
(** [spawn ?finally work] is a fiber [f] executing function [work]
    concurrently. The calling fiber will not terminate before [f] does and,
    depending on the scheduler, it {e may} yield control at that point.

    [finally] is called unconditionally just before the fiber
    terminates including if the fiber is aborted before [work] gets a
    chance to execute. It follows the same discipline as the [finally]
    argument of {!Fun.protect}: it's a programming error to raise an
    exception in it; case arising {!Finally_raised} is raised and
    trapped. *)

(** {1:ids Identifiers} *)

type id = int
(** The type for fiber unique identifiers. *)

val id_nil : id
(** [id_nil] is an identifier that will never be attributed to a fiber. *)

val id : 'a t -> id
(** [id f] is a unique identifier for the fiber. *)

(** {1:ret Return value}

    These functions provide access to the value returned by fibers in
    various ways. *)

type 'a return = 'a option
(** The type for fiber return values of type ['a]. [None] is returned
    if the fiber is aborted. *)

val poll : 'a t -> 'a return option
(** [poll f] polls the return value of [f]. This is:
    {ul
    {- [None] if the fiber is still executing.}
    {- [Some None] if the fiber is terminated and was aborted.}
    {- [Some (Some v)] if the fiber is terminated and returned value [v].}} *)

val join : 'a t -> 'a return
(** [join f] waits for [f] to terminate and returns its value. This is
    [None] in case [f] aborted and [Some v] in case [f] returned with
    value [v]. *)

val first : 'a t -> 'b t -> ('a return, 'b return) Either.t
(** [first f0 f1] waits for [f0] or [f1] to terminate and returns the
    value of the first one that did. If both are already terminated
    the value of [f0] is returned. *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t return
(** [either f0 f1] waits for [f0] or [f1] to terminate, returns the
    value of the first one that does with a value and aborts the fiber
    that didn't (if not terminated yet). If both are already
    terminated with some value, the value of [f0] is returned.  This
    is [None] if and only if both [f0] and [f1] aborted. *)

(** {1:abort Aborting} *)

exception Abort
(** The exception thrown into aborting fibers. Catching this exception
    is only for cleaning up, you must re-raise it afterwards;
    {!Fun.protect} does that for you. *)

val abort : 'a t -> unit
(** [abort f] aborts the fiber [f] (and its spawns). If [f]
    is already terminated this has no effect. *)

val self_abort : unit -> 'b
(** [self_abort ()] aborts the calling fiber. *)

val aborted : unit -> 'a t
(** [aborted ()] is an aborted fiber from outer space. *)

(** {1:blocking Blocking} *)

(** Existential fibers. *)
module E : sig

  (** *) (* odoc bug, need to report *)

  (**/**)
  val main : ?finally:(unit -> unit) -> (unit -> 'a) -> 'a t * (unit -> unit)
  (** [main func] is an existential fiber for [func] and its
      thread function (which has the termination handlers).
      Use as a toplevel fiber when you define your own scheduler. *)
  (**/**)

  type t
  (** The type for existential fibers. *)

  val id : t -> id
  (** [id f] is the unique identifier of [f]. *)

  val equal : t -> t -> bool
  (** [equal f0 f1] is [true] if [f0] and [f1] are the same fibers. *)

  val compare : t -> t -> int
  (** [compare f0 f1] is a total order on fibers compatible with {!equal}. *)

  val attach : t -> spawn:t -> unit
  (** [attach p ~spawn] makes [spawn] a spawn of [p]. *)

  val active_spawns : t -> t list
  (** [active_spawns f] are the possibly non-terminated spawns of [f]. *)

  val set_aborting : t -> unit
  (** [set_aborting f] sets the state of [f] to aborting, unless
      already terminated. *)

  val state : t -> [ `Running | `Aborting | `Terminated ]
  (** [state f] is the state of [f]. *)
end

val block :
  block:(E.t -> unit) -> abort:(E.t -> unit) -> retv:(E.t -> 'a) -> 'a
(** [block ~block ~abort ~retv] blocks the calling fiber [c] on a blocking
    operation with:
    {ul
    {- The function [block c] is immediately invoked by the fiber scheduler
       unless [c] is already aborting in which case [abort] is called.
       This should register the blocking  operation with an external entity
       reponsible for unblocking it when the operation result is available.
       If [block c] raises, the exception is directly thrown into the
       fiber [c].}
    {- The function [abort c] is invoked in case [c] gets aborted while waiting
       on the operation. It is invoked before [Abort] gets raised in [c].
       If the function raises, the exception is trapped.}
    {- The function [retv c] is called to get the operation's value once
       it no longer blocks. This value is used to continue [c]. If the
       function raises, the exception is thrown into [c].}}

    Eventually only one of [retv c] or [abort c] is called. Note that while
    running fibers nothing will ever unblock unless you provide an adequate
    {!type-unblock} function to {!val-run}. If none is provided the
    built-in one simply aborts blocked fibers. *)

type unblock = poll:bool -> E.t option
(** The type for functions to unblock blocked fibers. An [unblock]
    function is called by the scheduler as follows:
    {ul
    {- [unblock ~poll:true], the function should return a previously blocked
       fiber that no longer blocks, if any. The call must not block if there
       is no such fiber as there are other fibers that are willing to run.}
    {- [unblock ~poll:false], the function must return a peviously blocked
       fiber that no longer blocks. If there is none, it can block for as
       long as it wishes as there are no fiber to run. If it returns [None]
       it will be called again, which amounts to busy waiting.}}

    The function must not raise. If it does the exception is trapped
    and [None] is returned. *)

(** {1:run Running} *)

val run :
  ?unblock:unblock -> ?finally:(unit -> unit) -> (unit -> 'a) -> 'a option
(** [run main] creates a toplevel fiber with [main] and runs it to
    completion. [None] is returned if the fiber aborted.

    [finally] is called unconditionally before the function returns,
    see discussion in {!val-spawn}.

    [unblock] is the function called by the scheduler to unblock
    fibers that are {{!blocking}blocked}. If unspecified a default
    unblock handler that aborts blocked fibers is used.

    To devise your own scheduler see the {{!low}low-level} interface. *)

(** {1:low Low-level interface} *)

(** {2:effects Effects}

    In the following we call {e fiber thread} a fiber function wrapped
    with the termination handling code. *)

type spawn =
  { fiber : E.t; (** The spawned fiber. *)
    run : unit -> unit (** The fiber thread. *) }
(** The type for fiber spawns. *)

type 'a block =
  { block : E.t -> unit; (** Called by the fiber scheduler to register the
                             blocking operation with an external entity. *)
    abort : E.t -> unit; (** Called iff the blocked operation gets aborted. *)
    retv : E.t -> 'a; (** Called to get the value of the blocking operation
                           once it no longer blocks. *) }
(** The type for blocking operations returning values of type ['a]. See
    {!val-block} for more information on the functions. *)

type _ Effect.t +=
| Yield : unit Effect.t (** The current fiber yields control. *)
| Spawn : spawn -> unit Effect.t (** The current fiber spawns [s] *)
| Abort' : E.t option -> unit Effect.t (** The current or [Some e] fiber
                                             must be aborted. *)
| Block : 'a block -> 'a Effect.t (** The current fiber blocks on [b]. *)
(** The type for fiber effects.

    {b XXX.} This is unlikely to be the final word. We will need to
    express conjunctive (for termination) and disjunctive (for either)
    waiting of one fiber on another to get rid of busy yielding.  If
    we move more termination logic to the scheduler an effect to
    signal fiber termination may also be needed. *)

(** {2:custom Custom scheduler}

    Your custom scheduler will have to maintain a {e current fiber}
    which is the one whose fiber thread is running. A toplevel initial
    fiber thread can be created via {!E.main}. Besides:

    {ul
    {- On [Spawn s], it is important to {!E.attach} the spawned fiber
       of [s] to the current fiber. The termination discipline (implemented
       in the fiber thread) relies on this.}
    {- On [Abort], [None] indicates to terminate the current fiber.
       Also, all non-terminated spawns of the aborted fiber must
       be aborted before terminating it. Use {!E.spawns}, {!E.state} and
       {!E.set_aborting} for that.}
    {- On [Block], you must follow the protocol outlined in
       {!val-block}.}} *)
