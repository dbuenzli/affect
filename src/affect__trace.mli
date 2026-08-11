(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Tracing asynchronous function activity.

    @canonical Affect.Fun.Async.Trace  *)

open Affect__action.Private
open Affect__async_fun.Private

(** {1:exec Execution context identifiers} *)

type thread_id = int
(** The type for {!Thread} identifiers. *)

type domain_id = int
(** The type for {!Domain} identifiers. *)

type exec_id =
  { thread_id : thread_id;
    (** Thread identifier to which the trace is attributed to.
        Normally, the thread that executes {!report}. *)

    sup : domain_id;
    (** Identifier of a "supervising" domain for [thread_id]. If {!index} is
        {ul
        {- [< 0], this is the domain in which [thread_id] runs.}
        {- [= 0], this is the domain identifier of a scheduler
           {{!Fun.Async.main}main domain} and [thread_id] is the {e main
           thread} of that main scheduler domain.}
        {- [> 0], this is the domain identifier of a scheduler
           {{!Fun.Async.main}main domain} and [thread_id] is the {e main
           thread} of a worker domain spawned by the
           main domain and indexed by [index] in the scheduler.}} *)

    index : int;
    (** See {!sup}. *) }
(** The type for execution context identifiers. Basically thread identifiers
    with more metadata to make scheduler traces more lightweight but without
    confusions if multiple schedulers or external entities are also running. *)

val current_thread_id : unit -> thread_id
(** [current_thread_id ()] captures the thread identifier of the caller. *)

val current_exec_id : unit -> exec_id
(** [current_exec_id ()] captures the execution context of the caller.
    It has the current thread and domain id and [index] is [-1].

    {b Warning.} If you call that in an asynchronous function it will not
    capture its scheduler. This is meant to be used by external scheduler
    entities that want to trace. *)

(** {1:traces Traces} *)

type payload =
| Action_block of Async_fun.Call.id * Action.Meta.t
(** Reported on a blocking {!Affect.Action.invoke}. *)
| Action_unblock of Async_fun.Call.id * Action.Meta.t
(** Reported when a blocking {!Affect.Action.invoke} unblocks. *)
| Async_fun_call of { parent : Async_fun.Call.id; id : Async_fun.Call.id }
  (** Reported on {!Affect.Fun.Async.call}. *)
| Async_fun_continue of Async_fun.Call.id
  (** Reported when a function continues its execution after it was
      called, yielded or blocked. *)
| Async_fun_yield of Async_fun.Call.id
  (** Reported on {!Affect.Fun.Async.yield}. *)
| Async_fun_return of Async_fun.Call.id
  (** Reported when an asynchronous function returns. *)
| Async_fun_user of Async_fun.Call.id * string
  (** User defined trace, see {!Affect.Fun.Async.trace}. *)
| Domain_start (** Reported when domain starts its local scheduler. *)
| Domain_sleep (** Reported when a domain starts sleeping. *)
| Domain_unblocker of { block : bool }
  (** Reported when a domain executes an unblocker. *)
| Domain_resume
(** Reported when a domain returns from sleep or the unblocker *)
| Domain_panic of string
  (** Reported when a domain has an internal scheduler error. *)
| Domain_stop (** Reported when a domain stops its local scheduler. *)
(** The type for trace payloads. *)

type t = exec_id * payload
(** The type for traces. *)

val is_domain : t ->  bool
(** [is_domain t] is [true] iff the trace [t] only pertains to
    the life cycle of domains.  *)

val is_fun : t -> bool
(** [is_fun t] is [not (is_domain t)] and [true] iff the trace [t]
    only pertains to asynchronous function activity or actions. *)

val is_user : t -> bool
(** [is_user t] is [true] iff [t] is {!Async_fun_user}, that
    is the calls to {!Fun.Async.trace}. *)

(** {1:reporters Reporters} *)

type reporter = t -> unit
(** The type for reporters. *)

val default_reporter : reporter
(** [default_reporter] is the default reporter ({!Stdlib.ignore}). *)

val format_reporter : (unit -> Format.formatter) -> reporter
(** [format_reporter ppf] is a reporter using a synchronization safe
    formater [ppf] (see {!Format.get_std_formatter}) and {!pp} to report. *)

val stderr_reporter : reporter
(** [stderr_reporter] is [format_reporter (Format.get_err_formatter)]. *)

(** {2:current Current} *)

val reporter : unit -> reporter
(** [reporter] is the current reporter. *)

val set_reporter : reporter -> unit
(** [set_reporter reporter] sets the current reporter to [reporter] *)

val exchange_reporter : reporter -> reporter
(** [exchange_reporter r] sets the current reporter [r] and returns the previous
    reporter. *)

val with_reporter : reporter -> (unit -> 'a) -> 'a
(** [with_reporter r f] executes [f ()] with current reporter [r] and
    restores the previous reporter after [f] returns or raises.

    {b Warning.} There is no notion of scope here, the reporter is a global
    variable. If someone {!set_reporter} in parallel while in [f] it will
    be seen by [f]. *)

(** {2:filtering Filtering} *)

val keep : (t -> bool) -> reporter -> reporter
(** [keep sat reporter] reports with [reporter] but only those [sat]
    satisfying trace are given to it. *)

val only_fun : reporter -> reporter
(** [only_fun reporter] is [keep is_fun reporter]. *)

val only_user : reporter -> reporter
(** [only_user reporter] is [keep is_user reporter]. *)

(** {2:delayed Delayed} *)

(** Delayed reporters.

    May be useful for heisen bugs that vanish on printing. *)
module Delayed : sig

  type trace := t

  type t
  (** The type for delayed reporters. Those accumulate
      traces in memory. *)

  val make : unit -> t
  (** [make ()] is a new delayed reporter. You need to install
      it with {!val-reporter} and {!set_reporter}. *)

  val reporter : t -> reporter
  (** [reporter d] is the reporter of [d]. *)

  val clear : t -> unit
  (** [clear d] clears traces from [d]. *)

  val traces : t -> trace list
  (** [traces d] is are the traces kept by [d]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats the traces kept by [d]. *)

  val prerr : t -> unit
  (** [prerr] formats the traces kept by [d] on [stderr]. *)
end

(** {1:reporting Reporting} *)

val report : t -> unit
(** [report t] reports trace [t] on the {{!section-current}current} reporter. *)

(** {1:formatting Formatting} *)

val pp_exec_id : Format.formatter -> exec_id -> unit
(** [pp_exec_id] formats execution context identifiers for inspection. *)

val pp_exec_trace : Format.formatter -> exec_id -> unit
(** [pp_exec_trace] formats a header for execution contexts. *)

val pp_fun_trace : Format.formatter -> (exec_id * Async_fun.Call.id) -> unit
(** [pp_fun_trace] is like {!pp_exec_trace} but the trace also mentions
    the given call identifier. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats a trace. *)
