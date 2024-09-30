(*---------------------------------------------------------------------------
   Copyright (c) 2024 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Parallel asynchronous function calls.

    Read the short {{!concurrency_model}concurrency model}. *)

(** {1:fibers Fibers} *)

(** Fiber unique identifiers. *)
module Id : sig
  type t = int
  (** The type for fiber unique identifiers. *)

  val nil : t
  (** [nil] is an identifier that will never be attributed to a fiber. *)

  val equal : t -> t -> bool
  (** [equal] tests identifiers for equality. *)

  val compare : t -> t -> int
  (** [compare] is a total order on identifiers compatible with
      {!equal}.  *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats fiber identifiers. *)
end

type priority =
| Low (** Background computation. *)
| Normal (** Normal computation. *)
| High (** Urgent or interactive computation. *)
(** The type for fiber priorities. *)

type 'a t
(** The type for fibers. An asynchronous function call
    returning a value of type ['a]. *)

val async : ?only_main:bool -> ?priority:priority -> (unit -> 'a) -> 'a t
(** [async f] is a fiber executing function [f] in parallel to the
    caller. The caller scope will not return or raise before [f]
    returns or raises. [priority] is the execution priority, it
    defaults to the caller's priority. If [only_main] is [true], the
    scheduler ensure that the function only gets excecuted on the main
    thread (defaults to [false]).*)

val from_val : 'a -> 'a t
(** [from_val v] is [async (Fun.const v)]. *)

val yield : unit -> unit
(** [yield ()] cooperatively suspends the executing fiber.  *)

(** {1:awaiting Awaiting}

    {b Note.} Awaiting on fibers can always raise. If you want to guard
    against this, use the {{!trapping_exn}trapping combinators}
    before awaiting them. *)

val await : 'a t -> 'a
(** [await f] blocks until the asynchronous function of [f] returns or
    raises. *)

val await_all : 'a t list -> 'a list
(** [await_all fs] blocks until {e all} [fs] return or raise. Raises the
    leftmost exception if one of the fiber raises (including {!Cancelled}). *)

val await_first : 'a t list -> 'a * 'a t list
(** [await_first fs] awaits the first, leftmost, [fs]
    that returns or raises (including {!Cancelled}). The returned list (if any)
    is [fs], in the same order, without the fiber that returned. Raises
    [Invalid_argument] if the list is empty. *)

val await_either : 'a t -> 'b t -> ('a, 'b) Either.t
(** [await_either f0 f1] awaits the first, leftmost, fiber that returns or
    raises (including {!Cancelled}). *)

val poll : 'a t -> 'a option
(** [poll f] is [None] if [f] is still running and it's return value or raise
    otherwise. *)

(** {2:picking Picking}

    Picking is for selecting among competing computations. *)

val pick_first : 'a t list -> 'a
(** [pick_first fs] awaits the first of [fs] that returns or raises a
    non {!Cancelled} exception and {!cancel}s the other ones. Raises
    [Invalid_argument] if the list empty. *)

val pick_either : 'a t -> 'b t -> ('a, 'b) Either.t
(** [pick_either f0 f1] awaits the first, lefmost, of [f0] and [f1] that returns
    or raises a non {!Cancelled} exception and {!cancel}s the other one. *)

(** {1:canceling Cancelling} *)

exception Cancelled
(** Exception thrown to indicate that a fiber is cancelled. *)

val cancel : 'a t -> unit
(** [cancel f] marks the fiber [f] and its current and future asynchronous
    function calls as being cancelled. If [f] already returned this has no
    effect. *)

val self_cancel : unit -> unit
(** [self_cancel ()] cancels the executing fiber. *)

val self_is_cancelled : unit -> bool
(** [self_is_cancelled ()] is [true] if the excecuting fiber is cancelled. *)

val self_check_cancellation : unit -> unit
(** [self_check_cancellation] raises {!Cancelled} if
    [self_is_cancelled ()] is [true]. *)

(** {1:trapping_exn Trapping exceptions}

    A few conveniences to protect from raising fibers. Avoids
    multiplying the number of [await] functions. *)

val trap_user_exn : 'a t -> ('a, exn * Printexc.raw_backtrace) result t
(** [trap_user_exn f] is [f] but turns any exception
    except {!Cancelled},  {!Stack_overflow}, {!Out_of_memory} or
    {!Sys.Break} into [Error _]. *)

val trap_cancelled : 'a t -> 'a option t
(** [trap_cancelled f] is [f] but turns a {!Cancelled} exception into an
    option. *)

val trap_any_exn : 'a t -> ('a option, exn * Printexc.raw_backtrace) result t
(** [trap_any_exn] is [f] but turns a {!Cancelled} exception into [None] and any
    other exception except {!Stack_overflow}, {!Out_of_memory}, {!Sys.Break},
    into [Error _]. *)

(** {1:props Properties} *)

val id : 'a t -> Id.t
(** [id f] is the unique identifier of [f]. *)

val priority : 'a t -> priority
(** [priority f] is the priority of [f]. *)

val cancelled : 'a t -> bool
(** [cancelled f] is [true] iff [f] is marked as cancelled. *)

(** Existential fibers *)
module Handle : sig

  type 'a fiber := 'a t

  type t (* = V : 'a fiber -> t [@@unboxed] (* FIXME doesn't work. *) *)
  (** The type for exisential fibers. This is {!Fiber.t} with
      the ['a] hidden. *)

  val self : unit -> t
  (** [self] is a handle to the executing fiber. *)

  val id : t -> Id.t
  (** [id h] is the unique identifier of the fiber. *)

  val equal : t -> t -> bool
  (** [equal] tests fibers for equality. *)

  val compare : t -> t -> int
  (** [compare] is a total order on fibers compatible with {!equal}. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats exisential fibers. *)
end

val handle : 'a t -> Handle.t
(** [handle f] is the handle of f. *)

(** {1:blocking Blocking} *)

val block :
  block:(Handle.t -> unit) -> cancel:(Handle.t -> bool) ->
  return:(Handle.t -> 'a) -> 'a
(** [block ~block ~cancel ~return] blocks the calling fiber (hereafter
    [f]) on a blocking operation. The given functions are used as follows:

    {ul
    {- The function [block f] is immediately invoked by the scheduler.
       This should register the blocking operation with an external
       entity reponsible for unblocking it when the operation result
       is available.  If [block f] raises, the exception is directly
       thrown into the fiber [f] and not blocked.}
    {- The function [cancel f] is invoked in case [f] gets cancelled while
       blocked on the operation. If [true] is returned the scheduler unblocks
       [f] and throws {!Cancelled} into [f]. If [false] is returned
       the operation remains blocked and [return f] will be called once it
       is unblocked. If [cancel f] raises the scheduler unblocks [f] and
       throws the exception into [f].}
    {- The function [return f] is called to get the operation's value once
       it no longer blocks. This value is used to continue [f]. If [return f]
       raises the exception is thrown into [f].}}

    Take into account the following points:

    {ul
    {- In general it is recommended for blocking operations not be concerned
       about the cancellation status of the fiber they block (XXX for now
       {!Handle.t} does not even provide that).}
    {- It is recommended for blocking operations to raise {!Cancelled} if
       their [cancel] function is called. Either by returning [true] or
       by raising in [return] (Warning, if [cancelled f] is [true] in
       [return] it does not mean that [cancel] was called, it could
       have been blocked while being already cancelled).}
    {- The function [block] is guaranteed to be called by the domain
       executing the block. Other function may be called by other
       domains.}
    {- Correct schedulers always call these functions at most once. [block]
       is always called. If [cancel] is called and returns [true] or
       raises, [return] is never called.}}

    Note that nothing will ever unblock unless you provide an adequate
    {!type-unblock} function to {!val-main}. *)

val self_non_cancelling_blocks : (unit -> 'a) -> 'a
(** [self_non_cancelling_blocks f] ensures in [f ()] that blocking
    operations of the {e executing fiber} do not get notified of
    cancellation if it gets cancelled (in other words, [cancel] functions
    of {!block}s invoked by [f] never get called).

    {b Important.} Unlike {!cancel} and {!self_cancel} which propagate
    to the asynchronous calls of a fiber. This does not. Making blocks
    non-cancelling may be paramount to a fiber's correctness, so it
    has to remain in control of it. *)

(** {2:unblocking Unblocking} *)

type unblock = poll:bool -> Handle.t option
(** The type for functions to unblock blocked fibers. These functions
    need to be given to {!main}.

    An [unblock] function is called by the scheduler as follows:

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

val never_unblock : unblock
(** [never_unblock] nevers unblocks anything. Only use this if you are
    philosophizing. *)

val unblocks : unblock list -> unblock
(** [unblocks us] composes [us] by calling them one after the other in
    circular order for fairness. {b FIXME.} That doesn't work for
    [poll:false]. See {!todo}. *)

(** {1:run Running} *)

val main : ?domains:int -> unblock:unblock -> (unit -> 'a) -> 'a
(** [main f] creates a top level fiber with [f] and runs it to completion.
    {ul
    {- [unblock] is the function called by the scheduler to {{!unblocking}
       unblock}. If you are never blocking but only awaiting you can use
       {!unblock_none}.}
    {- [domains] is the number of domains to use. Defaults to
       to the value specified in the environment variable
       {!AFFECT_DOMAIN_COUNT} or if unparseable to
       {!Domain.recommended_domain_count}.}}

    Just interpose {!main} on your [main] function as follows:
    {[
      let main () =
        Fiber.main ~unblock:Funix.unblock @@ fun () ->
        …

      let () = if !Sys.interactive then () else exit (main ())
    ]} *)

(** {1:fmt Formatters} *)

val pp : Format.formatter -> 'a t -> unit
(** [pp ppf f] formats the fiber status [f] for inspection. *)

val pp' : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp' pp_v] is like {!pp} but uses [pp_v] to format the value if
    available. *)

val pp_id : Format.formatter -> 'a t -> unit
(** [pp_id ppf f] formats a short identifying header for [f] *)

val pp_value :
  (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
(** [pp_value pp_v f] formats the fiber value of [f] or a placeholder
    if its still running. *)

(** {1:low Low-level interface} *)

(** For devising your own scheduler.

    At some point.

    {b Warning.} This interface is subject to change even between
    minore versions of the library. *)
module Private : sig
end

(** {1:concurrency_model Concurrency model}

    There is no distinction between concurrency and parallelism.
    Fibers represent asynchronous function calls that execute in
    parallel to a calling function. We use the terms {e fiber} and {e
    asynchronous function call} interchangeably.

    The model is as follow:
    {ol
    {- Fibers are structured and aligned on function scopes. If a function
       makes asychronous function calls with {!async} it does not return or
       raise before all these subcalls return or raise.}
    {- Fibers are cooperative. They must progress. If they are unable
       to do so they must block. In this case the fiber execution is suspended
       and resumed after the operation is unblocked. A distinguished, built-in,
       blocking operation is {!await} which waits for an asynchronous
       function call to return or raise. Other than that, libraries provide
       suitable direct-style blocking functions that call {!block} underneath
       and an associated function to {!unblock} them that you
       specify for running your {!main} function.}
    {- Fibers are oblivious of their scheduling. Execution can be
       serialized or parallelized on an arbitrary number of domains or
       threads. Except for explicit requests to be only executed on the main
       thread, no assumption can be made on how they are scheduled
       by the running program.}
    {- Fibers have {{!type-priority}priorities}. By default, fibers
       inherit the priority of their caller. Priorities are scheduling
       {e hints}, they can be changed by schedulers. For example to avoid
       priority inversion when a high priority fiber awaits a low priority
       fiber.}
    {- Fibers can be marked for cancellation with {!cancel} or {!self_cancel}.}
    {- Cancellation is structured and aligned on function scopes. When
       a fiber is marked as cancelled, its own current and future
       asynchronous function calls are also marked as cancelled.}
    {- Cancellation is cooperative. It is up to
       the fiber itself to {{!self_is_cancelled}check} for cancellation
       and decide what it wants to do about it. For example it could simply
       return a partially computed result. However it is recommended to quickly
       release the resources it holds and terminate the function by raising
       {!Cancelled}.}
    {- Blocking operations can be cancelled. It is recommended for blocking
       operations not be concerned about the cancellation status of the
       fiber when they block or return. However unless
       {{!self_non_cancelling_blocks}prevented} by the fiber, a blocking
       operation can get notified if the fiber it blocks
       gets cancelled. In this case
       it is recommended for the blocking operation to unblock
       as soon as possible and return from it by raising {!Cancelled}.}}

    See also the {{!page-design}design notes}. *)
