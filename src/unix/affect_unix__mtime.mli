(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Monotonic time.

    This time is measured by sampling {{!the_monotonic_clock}this clock}.
    It increases monotonically and, in contrast to {!Ptime}, is not
    subject to operating system calendar time adjustments.

    @canonical Affect_unix.Mtime *)

open Affect

(** {1:spans Time spans} *)

(** Time spans. *)
module Span : sig

  (** {1:span Time spans} *)

  type t
  (** The type for non-negative monotonic time spans.

      They represent the difference between two monotonic clock
      readings with nanosecond precision (1e-9s) and can measure up to
      approximatevely 584 Julian year spans before silently rolling
      over (unlikely since this is in a single program run). *)

  val zero : t
  (** [zero] is a span of 0ns. *)

  val one : t
  (** [one] is a span of 1ns. *)

  val max_span : t
  (** [max_span] is a span of [2^64-1]ns. *)

  val add : t -> t -> t
  (** [add s s'] is [s] + [s']. {b Warning.} Rolls over on
      overflow. *)

  val abs_sub : t -> t -> t
  (** [abs_sub s s'] |[s] - [s']| is the absolute difference
      between [s0] and [s1]. *)

  val sat_sub : t -> t -> t
  (** [sat_sub s s'] is [s] - [s'] and floors at zero. *)

  (** {1:preds Predicates and comparisons} *)

  val equal : t -> t -> bool
  (** [equal s0 s1] is [true] if and only if [s0] is equal to [s1]. *)

  val compare : t -> t -> int
  (** [compare] totally orders spans by increasing duration. *)

  val is_shorter : t -> than:t -> bool
  (** [is_shorter span ~than] is [true] if and only if [span] lasts stricly
      less than [than]. *)

  val is_longer : t -> than:t -> bool
  (** [is_longer span ~than] is [true] if and only if [span] lasts stricly
      more than [than]. *)

  (** {1:const Durations} *)

  val ( * ) : int -> t -> t
  (** [n * dur] is [n] times duration [dur]. Does not check for
      overflow or that [n] is positive. *)

  val ( / ) : t -> int -> t
  (** [dur / n] is duration [dur] divided by [n]. Does not check for
      that [n] is positive, rounds towards 0. *)

  val ( + ) : t -> t -> t
  (** [s + s'] is [s] + [s']. {b Warning.} Rolls over on overflow. *)

  val ( - ) : t -> t -> t
  (** [s - s'] is [sat_sub s s'], floors at zero. *)

  val ns : t
  (** [ns] is a nanosecond duration, 1·10{^-9}s. *)

  val us : t
  (** [us] is a microsecond duration, 1·10{^-6}s. *)

  val ms : t
  (** [ms] is a millisecond duration, 1·10{^-3}s. *)

  val s : t
  (** [s] is a second duration, 1s. *)

  val min : t
  (** [min] is a minute duration, 60s. *)

  val hour : t
  (** [hour] is an hour duration, 3600s. *)

  val day : t
  (** [day] is a day duration, 86'400s. *)

  val year : t
  (** [year] is a Julian year duration (365.25 days), 31'557'600s. *)

  (** {1:conv Conversions} *)

  val to_uint64_ns : t -> int64
  (** [to_uint64_ns span] is [span] as an {e unsigned} 64-bit integer nanosecond
      span. *)

  val of_uint64_ns : int64 -> t
  (** [of_uint64_ns u] is the {e unsigned} 64-bit integer nanosecond span [u]
      as a span. *)

  val of_float_ns : float -> t option
  (** [of_float_ns f] is the positive floating point nanosecond span
      [f] as a span. This is [None] if [f] is negative, non finite, or
      larger or equal than 2{^53} (~104 days, the largest exact
      floating point integer). *)

  val to_float_ns : t -> float
  (** [to_float_ns span] is [span] as a nanosecond floating point span.
      Note that if [span] is larger than 2{^53} (~104 days, the largest
      exact floating point integer) the result is an approximation and
      will not round trip with {!of_float_ns}. *)

  val to_float_s : t -> float
  (** [to_float_s span] is [1e9 * ]{!to_float_ns}[ span]. *)

  val of_float_s : float -> t option
  (** [of_float_s f] is [of_float_ns (f *. 1e9)] *)

   (** {1:fmt Formatting}

       {b Note.} The [fmt] library has {!Fmt.uint64_ns_span} for actual
       prettyness. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats with {!Fmt.uint64_ns_span}. *)

  val pp_ns : Format.formatter -> t -> unit
  (** [pp_ns ppf s] prints [s] as an unsigned 64-bit integer nanosecond
      span. *)
end

(** {1:waiting Waiting}

    {b Warning.} This relies on an {!Unix.unblocker} being setup on
    the executing domain. *)

val wait_for : Span.t -> unit
(** [wait_for span] blocks the caller until a minimum amount of [span]
    time has elapsed on {{!the_monotonic_clock}the monotonic clock}.
    The actual supported resolution is unspecified and may depend on
    scheduling but millisecond waits should be reasonably precise.
    Use {!observe_wait_for} to measure the overshoot. *)

val observe_wait_for : Span.t -> Span.t
(** [observe_wait_for] is like {!wait_for} but returns the actual amount
    of monotonic time that elapsed on {{!the_monotonic_clock}the
    monotonic clock} between the call and when it returned. *)

(** {2:actions Actions} *)

val wait_for' : Span.t -> 'tag -> 'tag Action.t
(** [wait_for' span tag] is the action for {!wait_for}. The action
    invocation enables and synchronizes with [tag] after a minimum
    amount of [span] monotonic time has elapsed on
    {{!the_monotonic_clock}the monotonic clock}. *)

(** {1:counters Counters} *)

type counter
(** The type for monotonic wall-clock time counters. *)

val counter : unit -> counter
(** [counter ()] is a counter counting from now on. *)

val count : counter -> Span.t
(** [count c] is the amount of time that has elapsed on
    {{!the_monotonic_clock}the monotonic clock} since [c] was created. *)

val elapsed : unit -> Span.t
(** [elapsed ()] is the amount of time that has elapsed on
    {{!the_monotonic_clock}the monotonic clock} since the beginning of the
    program. *)

(** {1:timestamps Timestamps}

    {b Note.} Only use timestamps if you need inter-process time
    correlation, otherwise prefer {!elapsed} and {{!counters}counters} to
    measure time. *)

type t
(** The type for monotonic timestamps relative to an indeterminate
    system-wide event (e.g. last startup). Their absolute value has no
    meaning but can be used for inter-process time correlation. *)

val now : unit -> t
(** [now ()] is the current system-relative timestamp on
    {{!the_monotonic_clock}the monotonic clock}. Its absolute value is
    meaningless. *)

val min_stamp : t
(** [min_stamp] is the earliest timestamp. *)

val max_stamp : t
(** [max_stamp] is the latest timestamp. *)

(** {2:preds Predicates} *)

val equal : t -> t -> bool
(** [equal t0 t1] is [true] if and only if [t0] and [t1] are equal. *)

val compare : t -> t -> int
(** [compare] totally orders timestamps by increasing time. *)

val is_earlier : t -> than:t -> bool
(** [is_earlier t ~than] is [true] if and only if [t] occurred strictly
    before [than]. *)

val is_later : t -> than:t -> bool
(** [is_later t ~than] is [true] if and only if [t] occurred strictly after
    [than]. *)

(** {2:arith Arithmetic} *)

val span : t -> t -> Span.t
(** [span t0 t1] is the span between [t0] and [t1] regardless of the
    order between [t0] and [t1]. *)

val add_span : t -> Span.t -> t option
(** [add_span t s] is the timestamp [s] units later than [t] or [None] if
    the result overflows. *)

val sub_span : t -> Span.t -> t option
(** [sub_span t s] is the timestamp [s] units earlier than [t] or
    [None] if overflows. *)

(** {2:converting Converting} *)

val to_uint64_ns : t -> int64
(** [to_uint64_ns t] is [t] as an {e unsigned} 64-bit integer
    nanosecond timestamp. The absolute value is meaningless. *)

val of_uint64_ns : int64 -> t
(** [to_uint64_ns t] is [t] is an {e unsigned} 64-bit integer
    nanosecond timestamp as a timestamp.

    {b Warning.} Timestamps returned by this function should only be
    used with other timestamp values that are know to come from the
    same operating system run. *)

val pp : Format.formatter -> t -> unit
(** [pp] is a formatter for timestamps. *)

(** {1:the_monotonic_clock The monotonic clock}

    {ul
    {- Linux uses {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/clock_gettime.html}[clock_gettime]}
       with {{:https://www.man7.org/linux/man-pages/man3/clock_settime.3.html}
       CLOCK_BOOTTIME}. This means that sleep time is taken into account.}
    {- Platforms with a POSIX clock use
       {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/clock_gettime.html}[clock_gettime]}
       with CLOCK_MONOTONIC.}
    {- MacOS >= 10.12 uses
       {{:https://developer.apple.com/documentation/kernel/1646199-mach_continuous_time}[mach_continous_time]}, sleep time is taken into
       account. For MacOS < 10.12, {{:https://developer.apple.com/documentation/kernel/1462446-mach_absolute_time}[mach_absolute_time]} is used, sleep time
       is not taken into account.}
     {- Windows uses
       {{:https://msdn.microsoft.com/en-us/library/windows/desktop/aa373083%28v=vs.85%29.aspx}Performance counters}.}} *)
