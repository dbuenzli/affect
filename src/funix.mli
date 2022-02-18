(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(** Fiber friendly {!Unix} functions.

    Clients of these functions need to be run in a {!Fiber.val-run}
    function called with {!Funix.val-unblock}. *)

(** {1:unblock Fiber unblocking} *)

val unblock : Fiber.unblock
(** [unblock] is the function to unblock fibers blocked by the function
    of this module. You must use this function with {!Fiber.val-run}. *)

(** {1:signals Signals} *)

(** Signals

    {b XXX.} Just providing a bracket for now. Do direct style waiting. *)
module Signal : sig
  type t = int
  (** The type for signal numbers. *)

  val set : t -> Sys.signal_behavior -> (Sys.signal_behavior, string) result
  (** [set s b] is like {!Sys.signal} but does not raise exceptions. *)

  val set_noerr : t -> Sys.signal_behavior -> unit
  (** [set_noerr s b] is like {!Sys.set_signal} but ignores any error.. *)

  val with' :
    t -> Sys.signal_behavior -> (unit -> 'a) -> ('a, string) result
  (** [with' s b f] sets [s] to [b], calls [f] and restore
      the signal to its initial behaviour, however [f] returns. This
      is [Error msg] if setting up the signal failed. *)
end

(** {1:sleep Sleeping} *)

val sleep_s : float -> unit
(** [sleep_s dur] suspends the fiber for [dur] seconds. *)

(** {1:io File descriptors operations}

    If the file descriptor is in {{!Unix.set_nonblock}non-blocking mode},
    these functions block the fiber, not the domain thread.  *)

val read : Unix.file_descr -> bytes -> start:int -> len:int -> bool
(** [read fd b ~start ~len] reads [len] bytes from [fd] into [b]
    starting at [start] and returns [true]. Returns [false] if
    [len] bytes could not be read (i.e. end of file/stream was
    hit). The function handles signal interruptions ([EINTR]) by
    retrying. *)

val write : Unix.file_descr -> bytes -> start:int -> len:int -> unit
(** [write fd b ~start ~len] writes [len] bytes starting at [start]
    from [b] on [fd]. The function handles signal interruptions
    ([EINTR]) by retrying. *)

val accept :
  ?cloexec:bool -> Unix.file_descr -> Unix.file_descr * Unix.sockaddr
(** [accept fd] is a fiber friendly {!Unix.accept} which returns file
    descriptors is in non-blocking mode. The function handles signal
    interruptions ([EINTR]) by retrying. *)

val connect : Unix.file_descr -> Unix.sockaddr -> unit
(** [connect fd addr] is a fiber friendly {!Unix.connect}. The function
    handles signal interruptions ([EINTR]) by retrying. *)

val close_noerr : Unix.file_descr -> unit
(** [close_noerr fd] closes [fd] and ignores any error. Useful for
    {!Fun.protect} [finally] functions which must not raise. *)

(** {1:endpoint Socket endpoints} *)

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

val pp_endpoint : Format.formatter -> endpoint -> unit
(** [pp_socket_endpoint] formats an unspecified representation of endpoint
    values. *)

val socket_of_endpoint :
  endpoint -> Unix.socket_type ->
  (Unix.sockaddr option * Unix.file_descr * bool, string) result
  (** [socket_of_endpoint c] is [Ok (addr, fd, close)] with:
      {ul
      {- [addr], the address for the socket, if any.}
      {- [fd], the file descriptor for the socket. If [c] is [`Fd fd]
         this [fd] untouched. Otherwise [fd] is a new file descriptor set to
         {{!Unix.set_nonblock}non-blocking mode} and has
         {{!Unix.set_close_on_exec}close on exec} set to [true].}
      {- [close] is [true] if the caller is in charge of closing it. This
         is [false] iff [c] is [`Fd _].}} *)


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
