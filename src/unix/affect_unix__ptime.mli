(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** POSIX time.

    This time is measured by sampling {{!the_posix_clock}this clock}.

    {b Warning.} POSIX time does not increase monotonically,
    it is subject to operating system calendar time adjustements and
    can even go back in time. Use {!Mtime} if you want to wait for
    wall-clock time spans.

    {b Note.} This is a minimal version of the [ptime] library
    which has more tools to deal with POSIX timestamp (e.g. date-time values)
    with the same representation, head there for more.

    @canonical Affect_unix.Ptime *)

open Affect

type t
(** The type for picosecond precision POSIX timestamps in the range
    \[{!min_stamp};{!max_stamp}\]. Note that POSIX timestamps, and
    hence values of this type are by definition always on the UTC
    timeline. *)

(** {1:waiting Waiting}

    {b Warning.} This relies on an {!Unix.unblocker} being setup on
    the executing domain. *)

val wait_until : t -> unit
(** [wait_until t] waits until {{!the_posix_clock}the POSIX clock} can be
    caught being greater or equal to the timestamp [t]. The actual
    supported resolution is unspecified and may also depend on
    scheduling but POSIX millisecond precision should be supported.
    Use {!observe_wait_until} to measure the overshoot. *)

val observe_wait_until : t -> t
(** [observe_wait_until] is like {!wait_until} but returns the value of
    {{!the_posix_clock}the POSIX clock} when the function returns. *)

(** {2:actions Actions} *)

val wait_until' : t -> 'tag -> 'tag Action.t
(** [wait_until' t tag] is the action for {!wait_until}. The action
    invocation enables and synchronizes with [tag] when the
    {{!the_posix_clock}POSIX clock} can be caught to be greater or
    equal to the timestamp [t]. *)

(** {1:timespans POSIX spans} *)

(** POSIX time spans.

    {b WARNING.} A POSIX time span is not equal to an SI second based time
    span. *)
module Span : sig

  (** {1:spans POSIX time spans} *)

  type t
  (** The type for signed picosecond precision POSIX time spans. A value of
      this type represent the POSIX duration between two POSIX
      timestamps. *)

  val v : int * int64 -> t
  (** [v s] is like {!of_d_ps}[ s] but raises [Invalid_argument] if [s] is
      not in the right range. Use {!of_d_ps} to deal with untrusted
      input. *)

  val zero : t
  (** [zero] is the neutral element of {!add}. *)

  val of_d_ps : int * int64 -> t option
  (** [of_d_ps (d, ps)] is a span for the signed POSIX picosecond span [d] *
      86_400e12 + [ps]. [d] is a signed number of POSIX days and [ps]
      a number of picoseconds in the range \[[0];[86_399_999_999_999_999L]\].
      [None] is returned if [ps] is not in the right range. *)

  (**/**)
  val unsafe_of_d_ps : int * int64 -> t
  val unsafe_of_d_ps_option : (int * int64) option -> t option
  (**/**)

  val to_d_ps : t -> int * int64
  (** [to_d_ps d] is the span [d] as a pair [(d, ps)] expressing the
      POSIX picosecond span [d] * 86_400e12 + [ps] with
      [ps] in the range \[[0];[86_399_999_999_999_999L]\] *)

  val of_int_s : int -> t
  (** [of_int_s secs] is a span from the signed integer POSIX second
      span [secs]. *)

  val to_int_s : t -> int option
  (** [to_int_s d] is the span [d] as a signed integer POSIX second
      span, if [int]'s range can represent it (note that this
      depends on {!Sys.word_size}). Subsecond precision numbers are
      truncated. *)

  val of_float_s : float -> t option
  (** [of_float_s secs] is a span from the signed floating point POSIX
      second span [d]. Subpicosecond precision numbers are truncated.

      [None] is returned if [secs] cannot be represented as a span.
      This occurs on {!Stdlib.nan} or if the duration in POSIX
      days cannot fit on an [int] (on 32-bit platforms this means the
      absolute magnitude of the duration is greater than ~2'941'758
      years). *)

  val to_float_s : t -> float
  (** [to_float_s s] is the span [s] as floating point POSIX seconds.

      {b Warning.} The magnitude of [s] may not be represented exactly
      by the floating point value. *)

  val of_mtime_span : Affect_unix__mtime.Span.t -> t
  (** [of_mtime_span span] is a span from the monotonic span [span]. *)

  val to_mtime_span : t -> Affect_unix__mtime.Span.t option
  (** [of_mtime_span span] is the absolute value of [span] as
      a monotonic span or [None] if [span] cannot be represented. *)

  (** {1:predicates Predicates} *)

  val equal : t -> t -> bool
  (** [equal d d'] is [true] if and only if [d] and [d'] are the same time
      span. *)

  val compare : t -> t -> int
  (** [compare d d'] is a total order on durations that is compatible
      with signed time span order. *)

  val is_shorter : t -> than:t -> bool
  (** [is_shorter span ~than] is [true] if and only if [span] lasts stricly
      less than [than]. *)

  val is_longer : t -> than:t -> bool
  (** [is_longer span ~than] is [true] if and only if [span] lasts stricly
      more than [than]. *)

  (** {1:arith Arithmetic}

      {b Note.} The following functions rollover on overflows. *)

  val neg : t -> t
  (** [neg d] is the span [d] negated. *)

  val add : t -> t -> t
  (** [add d d'] is [d] + [d']. *)

  val sub : t -> t -> t
  (** [sub d d'] is [d] - [d']. *)

  val abs : t -> t
  (** [abs d] is the absolute value of span [d]. *)

  (** {1:print Pretty printing} *)

  val pp_d_ps : Format.formatter -> t -> unit
  (** [pp_d_ps ppf d] prints an unspecified, approximative, representation
      of [d] on [ppf]. *)
end

(** {1:timestamps Timestamps} *)

val now : unit -> t
(** [now ()] is the current value of the {{!the_posix_clock}POSIX clock}. *)

val epoch : t
(** [epoch] is 1970-01-01 00:00:00 UTC. *)

val min_stamp : t
(** [min_stamp] is 0000-01-01 00:00:00 UTC, the earliest timestamp
    representable. *)

val max_stamp : t
(** [max_stamp] is 9999-12-31 23:59:59.999999999999 UTC, the latest timestamp
    representable. *)

(** {2:predicates Predicates} *)

val equal : t -> t -> bool
(** [equal t0 t1] is [true] if and only if [t0] and [t1] are the same
    timestamps. *)

val compare : t -> t -> int
(** [compare t0 t1] is a total order on timestamps that is compatible with
    timeline order. *)

val is_earlier : t -> than:t -> bool
(** [is_earlier t ~than] is [true] if and only if [compare t than = -1]. *)

val is_later : t -> than:t -> bool
(** [is_later t ~than] is [true] if and only if [compare t than = 1]. *)

(** {2:arithmetic Arithmetic}

    {b WARNING.} A POSIX time span is not equal to an SI second based
    time span. Do not use these functions to perform calendar arithmetic or
    measure wall-clock durations, you will fail. *)

val diff : t -> t -> Span.t
(** [diff t t'] is the signed POSIX span [t - t'] that happens between
    the timestamps [t] and [t']. *)

val add_span : t -> Span.t -> t option
(** [add_span t d] is timestamp [t + d], that is [t] with the signed POSIX
    span [d] added. [None] is returned if the result is not in the
    range \[{!min_stamp};{!max_stamp}\]. *)

val sub_span : t -> Span.t -> t option
(** [sub_span t d] is the timestamp [t - d], that is [t] with the signed
    POSIX span [d] subtracted. [None] is returned if the result is not
    in the range \[{!min_stamp};{!max_stamp}\]. *)

(** {2:conversions Conversions} *)

val of_span : Span.t -> t option
(** [of_span d] is the POSIX time stamp that:
    {ul
    {- Happens at the POSIX span [d] {e after} {!epoch}
       if [d] is positive.}
    {- Happens at the POSIX span [d] {e before} {!epoch}
       if [d] is negative.}}
    [None] is returned if the timestamp is not in the range
    \[{!min_stamp};{!max_stamp}\]. *)

val to_span : t -> Span.t
(** [to_span t] is the signed POSIX span that happen between [t]
    and {!epoch}:
    {ul
    {- If the number is positive [t] happens {e after} {!epoch}.}
    {- If the number is negative [t] happens {e before} {!epoch}.}} *)

val of_float_s : float -> t option
(** [of_float_s d] is like {!of_span} but with [d] as a floating point
    second POSIX span [d]. This function is compatible with the result
    of {!Unix.gettimeofday}. Decimal fractional seconds beyond [1e-12]
    are truncated. *)

val to_float_s : t -> float
(** [to_float_s t] is like {!to_span} but returns a floating point second
    POSIX span.

    {b Warning.} Due to floating point inaccuracies do not expect the
    function to round trip with {!of_float_s}; especially near
    {!Ptime.min_stamp} and {!Ptime.max_stamp}. *)

val pp : Format.formatter -> t -> unit
(** [pp] formats an unspecified, approximative, representation
    of [timestamp], it looks like RFC 3339 but it's not do not use
    for serializing. *)

(** {1:the_posix_clock The POSIX clock}

    {ul
    {- Platforms with a POSIX clock (includes Linux) use
       {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/clock_gettime.html}[clock_gettime]} with [CLOCK_REALTIME].}
    {- On Darwin {{:http://pubs.opengroup.org/onlinepubs/9699919799/}
                  [gettimeofday]} is used.}
    {- On Windows
       {{:https://msdn.microsoft.com/en-us/library/windows/desktop/ms724390(v=vs.85).aspx}[GetSystemTime]}
       and
       {{:https://msdn.microsoft.com/en-us/library/windows/desktop/ms724421(v=vs.85).aspx}[GetTimeZoneInformation]}
       are used.}} *)
