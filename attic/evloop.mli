(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Event loop.

    This is a sample event loop that you might
    want to interface with effects. *)

(** Textual formatters.

    Helpers for dealing with {!Format}. *)
module Fmt : sig

  (** {1:formatting Formatting} *)

  val pf : Format.formatter -> ('a, Format.formatter, unit) format -> 'a
  (** [pf] is {!Format.fprintf}. *)

  val pr : ('a, Format.formatter, unit) format -> 'a
  (** [pf] is {!Format.printf}. *)

  val epr : ('a, Format.formatter, unit) format -> 'a
  (** [epr] is {!Format.eprintf}. *)

  val str : ('a, Format.formatter, unit, string) format4 -> 'a
  (** str is {!Format.asprintf}. *)

  val kpf :
    (Format.formatter -> 'a) -> Format.formatter ->
    ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [kpf] is {!Format.kfprintf}. *)

  val kstr : (string -> 'a) -> ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** kstr is {!Format.kasprintf}. *)

  val failwith : ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [failwith fmt ...] is [kstr failwith fmt ...] *)

  val failwith_notrace : ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [failwith_notrace] is like {!failwith} but [Failure] is raised with
      {!raise_notrace}. *)

  val invalid_arg : ('b, Format.formatter, unit, 'a) format4 -> 'b
  (** [invalid_arg fmt ...] is [kstr invalid_arg fmt ...] *)

  val error : ('b, Format.formatter , unit, ('a, string) result) format4 -> 'b
  (** [error fmt ...] is [kstr (fun s -> Error s) fmt ...] *)

  (** {1:formatters Formatters} *)

  type 'a t = Format.formatter -> 'a -> unit
  (** The type for formatters. *)

  val string : string t
  (** [string] is {!Format.pp_print_string}. *)
end

(** Result values *)
module Result : sig

  include module type of Stdlib.Result

  val to_failure : ('a, string) result -> 'a
  (** [to_failure r] is [failwith e] if [r] is [Error e] and [v]
      if [r] is [Ok v]. *)

  val to_invalid_arg : ('a, string) result -> 'a
  (** [to_invalid_arg r] is [invalid_arg e] if [r] is [Error e] and [v]
      if [r] is [Ok v]. *)

  (** let operators. *)
  module Syntax : sig
    val ( let* ) :
      ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
    (** [( let* )] is {!bind}. *)

    val ( and* ) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
    (** [( and* )] is {!product}. *)

    val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
    (** [( let+ )] is {!map}. *)

    val ( and+ ) : ('a, 'e) result -> ('b, 'e) result -> ('a * 'b, 'e) result
    (** [( and* )] is {!product}. *)
  end
end

(** Monotonic time stamps and spans.

    This module provides support for representing monotonic wall-clock time.
    This time increases monotonically and is not subject to operating
    system calendar time adjustement. Its absolute value is meaningless.

    To obtain monotonic time stamps and measure it use {!Os.Mtime}. *)
module Mtime : sig

  (** {1:span Monotonic time spans} *)

  type span
  (** The type for non-negative monotonic time spans. They represent
      the difference between two monotonic clock readings with
      nanosecond precision (1e-9s) and can measure up to
      approximatevely 584 Julian year spans before silently rolling
      over (unlikely since this is in a single program run). *)

  (** Monotonic time spans *)
  module Span : sig

    (** {1:span Time spans} *)

    type t = span
    (** See {!type:span}. *)

    val zero : span
    (** [zero] is a span of 0ns. *)

    val one : span
    (** [one] is a span of 1ns. *)

    val max_span : span
    (** [max_span] is a span of [2^64-1]ns. *)

    val add : span -> span -> span
    (** [add s0 s1] is [s0] + [s1]. {b Warning.} Rolls over on overflow. *)

    val abs_diff : span -> span -> span
    (** [abs_diff s0 s1] is the absolute difference between [s0] and [s1]. *)

    (** {1:preds Predicates and comparisons} *)

    val equal : span -> span -> bool
    (** [equal s0 s1] is [s0 = s1]. *)

    val compare : span -> span -> int
    (** [compare s0 s1] orders span by increasing duration. *)

    (** {1:const Durations} *)

    val ( * ) : int -> span -> span
    (** [n * dur] is [n] times duration [n]. Does not check for
        overflow or that [n] is positive. *)

    val ns : span
    (** [ns] is a nanosecond duration, 1·10{^-9}s. *)

    val us : span
    (** [us] is a microsecond duration, 1·10{^-6}s. *)

    val ms : span
    (** [ms] is a millisecond duration, 1·10{^-3}s. *)

    val s : span
    (** [s] is a second duration, 1s. *)

    val min : span
    (** [min] is a minute duration, 60s. *)

    val hour : span
    (** [hour] is an hour duration, 3600s. *)

    val day : span
    (** [day] is a day duration, 86'400s. *)

    val year : span
    (** [year] is a Julian year duration (365.25 days), 31'557'600s. *)

    (** {1:conv Conversions} *)

    val to_uint64_ns : span -> int64
    (** [to_uint64_ns s] is [s] as an {e unsigned} 64-bit integer nanosecond
        span. *)

    val of_uint64_ns : int64 -> span
    (** [of_uint64_ns u] is the {e unsigned} 64-bit integer nanosecond span [u]
        as a span. *)

    (** {1:fmt Formatting} *)

    val pp : span Fmt.t
    (** [pp] formats with {!Fmt.uint64_ns_span}. *)

    val pp_ns : span Fmt.t
    (** [pp_ns ppf s] prints [s] as an unsigned 64-bit integer nanosecond
        span. *)
  end

  (** {1:timestamp Monotonic timestamps}

      {b Note.} Only use timestamps if you need inter-process time
      correlation,  otherwise prefer {!Os.Mtime.elapsed} and
      {{!Evloop_std.Os.Mtime.monotonic_counters}counters} to measure time. *)

  type t
  (** The type for monotonic timestamps relative to an indeterminate
      system-wide event (e.g. last startup). Their absolute value has no
      meaning but can be used for inter-process time correlation. *)

  val to_uint64_ns : t -> int64
  (** [to_uint64_ns t] is [t] as an {e unsigned} 64-bit integer
      nanosecond timestamp. The absolute value is meaningless. *)

  val of_uint64_ns : int64 -> t
  (** [to_uint64_ns t] is [t] is an {e unsigned} 64-bit integer
      nanosecond timestamp as a timestamp.

      {b Warning.} Timestamps returned by this function should only be
      used with other timestamp values that are know to come from the
      same operating system run. *)

  val min_stamp : t
  (** [min_stamp] is the earliest timestamp. *)

  val max_stamp : t
  (** [max_stamp] is the latest timestamp. *)

  val pp : t Fmt.t
  (** [pp] is a formatter for timestamps. *)

  (** {1:preds Predicates} *)

  val equal : t -> t -> bool
  (** [equal t t'] is [true] iff [t] and [t'] are equal. *)

  val compare : t -> t -> int
  (** [compare t t'] orders timestamps by increasing time. *)

  val is_earlier : t -> than:t -> bool
  (** [is_earlier t ~than] is [true] iff [t] occurred before [than]. *)

  val is_later : t -> than:t -> bool
  (** [is_later t ~than] is [true] iff [t] occurred after [than]. *)

  (** {1:arith Arithmetic} *)

  val span : t -> t -> span
  (** [span t t'] is the span between [t] and [t'] regardless of the
      order between [t] and [t']. *)

  val add_span : t -> span -> t option
  (** [add_span t s] is the timestamp [s] units later than [t] or [None] if
      the result overflows. *)

  val sub_span : t -> span -> t option
  (** [sub_span t s] is the timestamp [s] units earlier than [t] or
      [None] if overflows. *)
end


(** Operating system interaction. *)
module Os : sig

  (** Signals *)
  module Signal : sig
    type t = int
    (** The type for signal numbers. *)

    val set : t -> Sys.signal_behavior -> (Sys.signal_behavior, string) result
    (** [set sg b] is like {!Sys.signal} but does not raise exceptions. *)

    val set_noerr : t -> Sys.signal_behavior -> unit
    (** [set_noerr sg b] is like {!Sys.set_signal} but ignores errors. *)
  end

  (** Unix file descriptor helpers *)
  module Fd : sig

    (** {1:closing Closing} *)

    val close_noerr : Unix.file_descr -> unit
    (** [close_noerr fd] closes [fd] and ignores any error.
        Useful for {!Fun.protect} [finally] functions which must not
        raise. *)

    (** {1:read_write Read and write}

        {b Note.} These functions may raise {!Unix_error}. *)

    val read : Unix.file_descr -> bytes -> start:int -> len:int -> bool
    (** [read fd b ~start ~len] reads [len] bytes from [fd] into [b]
        starting at [start] and returns [true]. Returns [false] if
        [len] bytes could not be read (i.e. end of file/stream was
        hit). The function handles signal interruptions ([EINTR]). *)

    val write : Unix.file_descr -> bytes -> start:int -> len:int -> unit
    (** [write fd b ~start ~len] writes [len] bytes starting at [start]
        from [b] on [fd]. The function handles signal interruptions
        ([EINTR]). *)

    (** {1:endpoint Socket endpoint specification} *)

    type endpoint =
    [ `Host of string * int (** Hostname and port. *)
    | `Sockaddr of Unix.sockaddr (** Given socket address. *)
    | `Fd of Unix.file_descr (** Direct file descriptor. *) ]
    (** The type for specifying a socket endpoint to connect to
        or to listen to on. *)

    val endpoint_of_string :
      default_port:int -> string -> (endpoint, string) result
    (** [connection_of_string ~default_port s] parses a connection
        specification from [s].  The format is [ADDR[:PORT]] or [PATH]
        for a Unix domain socket (detected by the the presence of
        a {{!Stdlib.Filename.dir_sep}directory separator}).
        [default_port] port is used if no [PORT] is specified. *)

    val pp_endpoint : endpoint Fmt.t
    (** [pp_endpoint] formats an unspecified representation of endpoint
        values. *)

    val socket_of_endpoint :
      endpoint -> Unix.socket_type ->
      (Unix.sockaddr option * Unix.file_descr * bool, string) result
    (** [socket_of_endpoint c] is [Ok (addr, fd, close)] with:
        {ul
        {- [addr], the address for the socket, if any.}
        {- [fd], the file descriptor for the socket.}
        {- [close] is [true] if the client is in charge of closing it.}}

        Unless [c] was [`Fd _], [fd]
        has {{:Unix.set_close_on_exec}close on exec} set to [true]. *)

    (** {1:sets Sets of file descriptors} *)

    (** Sets of file descriptors. *)
    module Set : sig
      include Set.S with type elt := Unix.file_descr
    end
  end

  (** IO and timeout events. *)
  module Ev : sig

    (** {1:sets Sets of events} *)

    type set
    (** The type for sets of events. *)

    val set : unit -> set
    (** [set ()] is a new empty set of events. *)

    val next : set -> [`Signal | `Event of (unit -> unit)] option
    (** [next s] blocks until the next event of [s] or a signal occurs. This is:
        {ul
        {- [None] if there are no longer any events in [set]}
        {- [Some (`Event cb)] if an event occured, [cb] is the function to
           invoke to handle the event.}
        {- [Some `Signal] if a signal occured.}} *)

    (** {1:events Events} *)

    type t
    (** The type for events. Represents a {{!section-timeout}timeout} or
        {{!section-fd}file descriptor} event. *)

    (** {2:timeout Timeout events} *)

    val timeout : set -> dur:Mtime.span -> (unit -> unit) -> t
    (** [timeout s ~dur cb] is the event in [s] that schedules the
        callback [cb] after a [dur] duration from now.

        {b Note.} Even if [dur] has nanosecond resolution, the
        maximal resolution is more likely to be microseconds, if not
        milliseconds. *)

    val timeout_exists : set -> bool
    (** [timeout_exists s] is [true] iff there is a timeout in [s]. *)

    (** {2:fd File descriptor events} *)

    type fd =
      [ `R (** Read opportunity event. *)
      | `W (** Write opportunity event. *) ]
    (** The type for file descriptor events. *)

    val fd : set -> Unix.file_descr -> fd -> (unit -> unit) -> t
    (** [fd s fd what cb] is the event in [s] that schedules the callback [cb]
        whenever a [what] can be performed on [fd]. *)

    val fd_or_timeout :
      set -> dur:Mtime.span -> Unix.file_descr -> fd ->
      (expired:bool -> unit -> unit) -> t
    (** [fd_or_timeout s ~dur fd what cb] is the event in [s] that
        schedules the callback [cb ~expired:false] whenever a [what] can be
        performed on [fd] or [cb ~expired:true] if that does not occur before
        duration [dur] from now.

        {b Note.} Even if [dur] has nanosecond resolution, the maximal
        resolution is more likely to be microseconds, if not
        milliseconds. *)

    val fd_exists : set -> bool
    (** [fd_exists s] is [true] iff there is a fd event in [s]. *)

    (** {2:removing Removing events} *)

    val remove : set -> t -> unit
    (** [remove s ev] removes [ev] from [s]. The callback associated to [ev]
        is dropped and will not be returned by {!next}. *)

    val remove_fd : set -> Unix.file_descr -> unit
    (** [remove_fd s fd] remove (in the sense of {!remove}) all events
        for file descriptor [fd] in [s]. No callback concerning [fd] will
        be returned by {!next}. *)

    val remove_all : set -> unit
    (** [remove_all s] removes all events from [s]. {!next} returns
        [None]. *)
  end

  (** Monotonic time clock.

      See {!Evloop.Mtime} for a discussion about monotonic time. *)
  module Mtime : sig

    (** {1:monotonic_clock Monotonic clock} *)

    val now : unit -> Mtime.t
    (** [now ()] is the current system-relative monotonic timestamp. Its
        absolute value is meaningless. *)

    val elapsed : unit -> Mtime.span
    (** [elapsed ()] is the monotonic time span elapsed since the
        beginning of the program. *)

    (** {1:monotonic_counters Monotonic wall-clock time counters} *)

    type counter
    (** The type for monotonic wall-clock time counters. *)

    val counter : unit -> counter
    (** [counter ()] is a counter counting from now on. *)

    val count : counter -> Mtime.span
    (** [count c] is the monotonic time span elapsed since [c] was created. *)

    (** {1:err Error handling}

        The functions {!elapsed}, {!now}, {!val-counter},
        raise [Sys_error] whenever they can't determine the
        current time or that it doesn't fit in [Mtime]'s range. Usually
        this exception should only be catched at the toplevel of your
        program to log it and abort the program. It indicates a serious
        error condition in the system.

        {1:platform_support Platform support}

        {ul
        {- Platforms with a POSIX clock (includes Linux) use
        {{:http://pubs.opengroup.org/onlinepubs/9699919799/functions/clock_gettime.html}[clock_gettime]}
        with CLOCK_MONOTONIC.}
        {- Darwin uses
        {{:https://developer.apple.com/library/mac/qa/qa1398/_index.html}[mach_absolute_time]}.}
        {- Windows uses
        {{:https://msdn.microsoft.com/en-us/library/windows/desktop/aa373083%28v=vs.85%29.aspx}Performance counters}.}} *)
  end

end

(** Command line interface fragments *)
module Cli : sig

  (** {1:options Options} *)

  val endpoint :
    ?opts:string list -> ?docs:string -> default_port:int ->
    default_endpoint:Os.Fd.endpoint -> unit ->
    Os.Fd.endpoint Cmdliner.Term.t
  (** [endpoint] is an option for specifying a socket endpoint.
      {ul
      {- [default_port] is the default port when unspecified.}
      {- [default_endpoint] is the default endpoint when unspecified}
      {- [docs] is the section where the option is documented}
      {- [opts] are the options to use (defaults to [["s"; "socket"]])}} *)
end

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
