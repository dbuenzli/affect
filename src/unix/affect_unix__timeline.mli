(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Timelines to handle {!Affect_unix__ptime} and
    {!Affect_unix__mtime} waits.

    {b Note.} Our terminology looks cool, who doesn't want to
    progress time? But maybe we should reframe it to use the samee
    vocabulary as {!Affect_unix__fd.Fd.UNBLOCKER} and
    {!Affect_unix__signal.Unblocker}. *)

open Affect
open Affect.Action.Private

(** {1:mtime Monotonic clock} *)

type uint64 = int64
(** The type for unsigned 64-bit integers. *)

type mtime_ns = uint64
(** The type for monotonic timestamps with nanosecond precision. *)

type mtime_span_ns = uint64
(** The type for monotonic time spans with nanosecond precision. *)

val mtime_now_ns : unit -> mtime_span_ns
(** [mtime_now_ns ()] is the current value of the monotonic clock. *)

val mtime_max_stamp : mtime_ns
(** [mtime_max_stamp] is the maximal monotonic timestamp. *)

val mtime_compare : mtime_ns -> mtime_ns -> int
(** [mtime_compare] totally orders monotonic timestamps. *)

val mtime_add_span : mtime_ns -> mtime_span_ns -> mtime_ns option
(** [mtime_add_span t span] adds [span] to [t] and returns [None]
    on overflow. *)

val mtime_span_ns_to_float_s : mtime_span_ns -> float
(** [mtime_span_ns_to_float_s span] is [span] as a float in seconds. *)

val mtime_span_ns_to_int_ms : mtime_span_ns -> int
(** [mtime_span_ns_to_int_ms span] as an int in seconds. It rounds down
    towards [0] and returns [max_int] if the [int] can't represent the
    magnitude. *)

(** {1:ptime_clock POSIX clock} *)

type ptime_d_ps = int * int64
(** The type for POSIX timestamps with pico second precision. The number
    of POSIX days since the epoch and picosecond precision POSIX time point
    in that day starting from 00:00:00. *)

val ptime_now_d_ps : unit -> ptime_d_ps
(** [ptime_now_d_ps ()] is the current value of the POSIX clock. *)

val ptime_neg : ptime_d_ps -> ptime_d_ps
val ptime_add : ptime_d_ps -> ptime_d_ps -> ptime_d_ps
val ptime_sub : ptime_d_ps -> ptime_d_ps -> ptime_d_ps

(** {1:time Timelines} *)

type t
(** The type for timelines running on monotonic time. Timelines are
    synchronization safe. *)

val make : unit -> t
(** [make ()] is a new timeline that runs on monotonic time. *)

val is_empty : t -> bool
(** [is_empty timeline] is [true] if and only there is no
    deadline in [timeline]. *)

val add_mtime_deadline : t -> mtime_ns -> Action.Blocked.Value.t -> unit
(** [add_mtime_deadline timeline t blocked] adds a deadline on [timeline] to
    try to unblock [blocked] once {!mtime_now_ns}[ ()] is greater or
    equal to [t]. *)

val add_ptime_deadline :
  t -> now:ptime_d_ps -> ptime_d_ps -> Action.Blocked.Value.t -> unit
(** [add_ptime_deadline timeline t blocked] adds a deadline on [timeline]
    to try to unblock [blocked] once {!ptime_now_d_ps} can be caught to be
    greater or equal to [t]. [now] is the timestamp that was just
    checked to to be earlier than [t].

    The deadline is computed by diffing [t] with the current {!ptime_now_d_ps}
    and converting the resuling POSIX time span which is interepreted as
    monotonic time span (which is generally incorrect due to presence of leap
    seconds) which it adds to the current {!mtime_now_ns} to yield
    the deadline. When the deadline is reached, [t] is checked to be smaller
    or equal to {!ptime_now_d_ps}, if not it is rescheduled using the same
    procedure. *)

val progress_to_next_deadline : t -> bool * mtime_span_ns option
(** [progress_to_next_deadline timeline] unblocks all expired deadlines on
    [timeline], returns [true] if blocked actions were unblocked and
    the timestamp to the next deadline (if any). *)

(** {2:domain_local_timeline Domain local timeline} *)

val get_domain_local : unit -> t
(** [get_domain_local ()] gets the domain local timeline (which is usually
    shared among the domains of a scheduler). This raises
    [Invalid_argument] if not timeline was set with {!set} or if it
    was {!clear}ed. *)

val set_domain_local : t -> unit
(** [set_domain_local t] sets the domain local timeline on this domain
    to [t]. *)

val clear_domain_local : unit -> unit
(** [clear_domain_local ()] clears the domain local timeline on this domain. *)
