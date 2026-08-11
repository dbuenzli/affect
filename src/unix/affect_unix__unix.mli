(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** [Unix] module overriden with cooperative functions and actions.

    {ul
    {- The module tries to be as faithfull as possible to the OCaml {!Unix}
       module so that there are few surprises when one is swapped for the
       other.}
    {- Using this module does not automatically make all of its functionality
       cooperative, some functions may still block your asynchronous functions.
       Cooperativeness is gradually added on best-effort basis, see
       {{!altered_functions}the list of
       functions} that have their behaviour changed.}}

    {b Warning.} Most of the functionality of this module relies on an
    {!Unix.unblocker} being setup on the executing domain.

    @canonical Affect_unix.Unix *)

open Affect

(** The full [Unix] module from the OCaml [unix] library is
    included here. It is hidden from the docs due to a suboptimal reading
    experience.
    {[
      include module type of Unix
        with type file_descr = Unix.file_descr
         and …
    ]}
*)

(**/**)
include module type of Unix
  with type error = Unix.error
   and type process_status = Unix.process_status
   and type wait_flag = Unix.wait_flag
   and type file_descr = Unix.file_descr (** @canonical Unix.file_descr *)
   and type seek_command = Unix.seek_command
   and type file_kind = Unix.file_kind
   and type stats = Unix.stats
   and type LargeFile.stats = Unix.LargeFile.stats
   and type access_permission = Unix.access_permission
   and type lock_command = Unix.lock_command
   and type sigprocmask_command = Unix.sigprocmask_command
   and type process_times = Unix.process_times
   and type tm = Unix.tm
   and type interval_timer = Unix.interval_timer
   and type interval_timer_status = Unix.interval_timer_status
   and type passwd_entry = Unix.passwd_entry
   and type group_entry = Unix.group_entry
   and type inet_addr = Unix.inet_addr
   and type socket_domain = Unix.socket_domain
   and type socket_type = Unix.socket_type
   and type sockaddr = Unix.sockaddr
   and type shutdown_command = Unix.shutdown_command
   and type msg_flag = Unix.msg_flag
   and type socket_bool_option = Unix.socket_bool_option
   and type socket_int_option = Unix.socket_int_option
   and type socket_optint_option = Unix.socket_optint_option
   and type socket_float_option = Unix.socket_float_option
   and type host_entry = Unix.host_entry
   and type protocol_entry = Unix.protocol_entry
   and type service_entry = Unix.service_entry
   and type addr_info = Unix.addr_info
   and type getaddrinfo_option = Unix.getaddrinfo_option
   and type name_info = Unix.name_info
   and type getnameinfo_option = Unix.getnameinfo_option
   and type terminal_io = Unix.terminal_io
   and type setattr_when = Unix.setattr_when
   and type flush_queue = Unix.flush_queue
   and type flow_action = Unix.flow_action
(** @closed *)
(**/**)

(** {1:io File descriptor operations} *)

val close_noerr : Unix.file_descr -> unit
(** [close_noerr] is like {!Unix.close} but never raises. *)

(** {2:sockets Sockets} *)

val socket : ?cloexec:bool -> Unix.socket_domain -> Unix.socket_type -> int ->
  Unix.file_descr
(** [socket] is like {!Unix.socket} except it has [cloexec] set to
    [true] by default and it sets the socket to non-blocking mode
    with {!Unix.set_nonblock}. *)

val accept : ?cloexec:bool -> Unix.file_descr -> Unix.file_descr * Unix.sockaddr
(** [accept] is a cooperative {!Unix.accept}. *)

val connect : Unix.file_descr -> Unix.sockaddr -> unit
(** [connect] is a cooperative {!Unix.connect}. *)

(** {2:reads Reads} *)

val read : Unix.file_descr -> bytes -> int -> int -> int
(** [read] is a cooperative {!Unix.read}. *)

val read_bigarray :
  Unix.file_descr ->
  (_, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int -> int -> int
(** [read_bigarray] is a cooperative {!Unix.read_bigarray}. *)

(** {2:writes Writes} *)

val write : Unix.file_descr -> bytes -> int -> int -> int
(** [write] is a cooperative {!Unix.write}. *)

val write_bigarray :
  Unix.file_descr ->
  (_, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int -> int -> int
(** [write_bigarray] is a cooperative {!Unix.write_bigarray}. *)

val write_substring : Unix.file_descr -> string -> int -> int -> int
(** [write_substring] is a cooperative {!Unix.write_substring}. *)

val single_write : Unix.file_descr -> bytes -> int -> int -> int
(** [single_write] is a cooperative {!Unix.single_write}. *)

val single_write_bigarray :
  Unix.file_descr ->
  (_, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t ->
  int -> int -> int
(** [single_write_bigarray] is a cooperative {!Unix.single_write_bigarray}. *)

val single_write_substring :
  Unix.file_descr -> string -> int -> int -> int
(** [single_write_substring] is a cooperative {!Unix.single_write_substring}. *)

(** {2:action Actions} *)

val wait_readable : Unix.file_descr -> 'tag -> 'tag Action.t
(** [wait_readable fd tag] is the action used by the {{!reads}read
    operations} and {!accept}. The action invocation is enabled and
    synchronizes with [tag] whenever the non-blocking file descriptor
    [fd] becomes available for reading. *)

val wait_writable : Unix.file_descr -> 'tag -> 'tag Action.t
(** [wait_writable fd tag] is the action used by the {{!write}write
    operations} and {!connect}. The action invocation is enabled and
    synchronizes with [tag] whenever the non-blocking file descriptor
    [fd] becomes available for writing. *)

(** {1:signals Signals} *)

(** Waiting on signals.

    {b Warning.} This uses the OCaml signal handlers of {!Sys} which
    are global variables. If you don't get what you want check that no
    other part of the program is trying to {!Sys.signal} or
    {!Sys.set_signal}.

    {b Implementation note.} Theoretically this should work reliably
    regardless on how signal delivery is configured in the program.
    The {!Signal.Waiters} signal handling mode integrates into the file
    descriptor watching infrastructure via a socket created with
    {!Unix.socketpair} (self-pipe trick). *)
module Signal : sig

  (** {1:waiting Waiting}  *)

  val wait : Sys.signal -> unit
  (** [wait signal] waits for the next occurence of [signal] and continues
      if and only if the {!handler} of [s] is {!Waiters} at signal occurence
      time. *)

  val wait_any : Sys.signal list -> Sys.signal
  (** [wait_any signals] waits for the next occurence of a signal in
      [signals] whose handler is {!Waiters} at occurence time and
      continues with the signal that did. *)

  (** {2:action Actions} *)

  val wait' : Sys.signal -> 'tag -> 'tag Action.t
  (** [wait' signal tag] is the action used by {!wait} and {!wait_any}. The
      action invocation is enabled and synchronizes with [tag]
      whenever the next occurence of [signal] is delivered to the
      process and [signal] is set to be handled by {!Waiters} at that
      instant. *)

(** {1:handling Handling} *)

  type handler =
  | Default
    (** Default signal hander, usually abort the program. *)
  | Ignore
    (** Ignore the signal. *)
  | Fun of (Sys.signal -> unit)
    (** Call the given function with the signal number. *)
  | Waiters
    (** Unblock functions waiting on the signal with the {!wait'} action. *)

  val set : Sys.signal -> handler -> unit
  (** [set signal h] sets the handler of [signal] to [h]. *)

  val set_and_restore : Sys.signal -> handler -> (unit -> 'a) -> 'a
  (** [set_and_restore signal b f] gets the handler for [signal] as
      [current], sets the handler of [signal] to [b], calls [f ()] and
      sets the handler of [signal] back to [current] after [f]
      returned or raised.

      {b Warning.} Signal handlers are global, there is no notion
      of scope here. If someone sets [signal] in parallel these changes
      will also be visible in [f]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> Sys.signal -> unit
  (** [pp] formats signals for inspection. *)
end

(** {1:unblocking Unblocking the cooperants} *)

val unblocker : unit -> Action.unblocker
(** [unblocker ()] is an action unblocker to use with
    {!Affect.Fun.Async.main} for handling actions from
    {!Unix}, {!Mtime} and {!Ptime}. *)

val main :
  ?sigpipe:Signal.handler -> ?domain_spawn:((unit -> unit) -> unit Domain.t) ->
  ?domain_count:int -> ?schedule:Fun.Async.Schedule.t ->
  ?handler:Fun.Async.Call_handler.t -> (unit -> 'a) -> 'a
(** Invoking {!main} expands to:
    {[
    Unix.Signal.set_and_restore Sys.sigpipe sigpipe @@ fun () ->
    let unblocker = Unix.unblocker () in
    Fun.Async.main ~unblocker ?domain_spawn ?domain_count ?schedule ?handler f
    ]}
    The default of [sigpipe] is {!Unix.Signal.Ignore} which is the right
    default. *)

(** {1:changes Changes from the [Unix] module} *)

(** {2:altered_functions Altered functions}
    {ul
    {- {!accept}, assume a non-blocking file descriptor, handles
       [EWOULDBLOCK] and [EGAIN] by invoking {!wait_readable}.}

    {- {!connect}, assumes a non-blocking file descriptor, handle
       [EINPROGRESS] by invoking {!wait_readable} and after that
       checks if an error occcured with {!Unix.getsockopt_error}}

    {- {!socket}, after the socket is created it is directly set to
       non-blocking mode with a call to {!Unix.set_nonblock}.}

    {- {!read}, {!read_bigarray},
       {!write}, {!write_bigarray}, {!write_substring},
       {!single_write}, {!single_write_bigarray}, {!single_write_substring}
       assume a non-blocking file
       descriptor and handles [EWOULDBLOCK] and [EAGAIN] by invoking
       {!wait_readable} or {!wait_writable}.}} *)

(** {2:additions Additions}

    {ul
    {- The {!Signal} module is added.}} *)
