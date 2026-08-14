(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Networking.

    {b Warning.} This interface is expected to move back to
    the [more] library, it will no longer be distributed
    by this library in the future. *)

(** {1:endpoints Endpoints} *)

open Affect

(** Network endpoints. *)
module Endpoint : sig

  (** {1:endpoints Endpoints} *)

  type t =
  [ `Host of string * int (** Hostname and port. *)
  | `Sockaddr of Unix.sockaddr (** Direct socket address. *)
  | `Fd of Unix.file_descr (** Direct file descriptor. *) ]
  (** The type for network endpoints. *)

  val of_string : default_port:int -> string -> (t, string) result
  (** [of_string ~default_port s] parses a socket endpoint specification
      from [s].

      The format is [ADDR[:PORT]] or [PATH] for a Unix domain socket
      (detected by the the presence of a directory
      separator). [default_port] port is used if no [PORT] is
      specified. *)

  val with_port_of_sockaddr : Unix.sockaddr -> t -> t
  (** [with_port_of_sockaddr addr ep] makes [ep]'s port coincide with the
      port of [addr] iff both have a port. Otherwise this is [ep]
      itself. Mostly useful to adjust an endpoint whose port number
      was specified as [0] in order to get the one allocated by [bind(2)]. *)

  (** {1:fmt Formatting} *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats endpoints for inspection. *)
end

(** {1:connections Connections} *)

(** Network connections. *)
module Connection : sig

  (** {1:connections Connections} *)

  type t
  (** The type for connections. *)

  val open' :
    ?nonblock:bool -> ?socket_type:Unix.socket_type ->
    peer:Endpoint.t -> unit -> (t, string) result
  (** [open ~peer ()] opens a connection to a peer listening on [peer] with:
      {ul
      {- [nonblock] if [true] the socket is in non-blocking mode (default).}
      {- [socket_type] is the type of connection. Defaults to
         {!Unix.SOCK_STREAM} or the [fd]'s type if [peer] is [`Fd fd] – if
         [socket_type] is specified in this case this errors if [fd] is not of
         the specified type.}
      {- If a new file descriptor is created, it is with [cloexec] set to
         [true].}
      {- If [peer] is [`Fd fd] it is assumed to be connected. So the call
         just tries to honour the [nonblock] argument and gets
         it's peer and local address from the [fd]. Also note that in this case
         {!close_noerr} does not close [fd] it's your duty to do so.}}

      If a connection is returned it must always eventually be closed with
      {!close_noerr}. *)

  val try_open :
    ?nonblock:bool -> ?socket_type:Unix.socket_type -> peer:Endpoint.t ->
    unit -> (t option, string) result
  (** [try_open ~endpoint] is like {!open'} except [None] is returned
      if {!Unix.connect} errors with [ENOENT], [ECONNREFUSED] or
      [EHOSTUNREACH]. *)

  val close_noerr : t -> unit
  (** [close_noerr c] closes [c] without raising errors and can be safely
      called repeatedly. Note that if [peer c] is [`Fd fd] this does
      not close [fd]. *)

  (** {1:props Properties} *)

  val fd : t -> Unix.file_descr
  (** [fd c] is the file descriptor of [c]. *)

  val peer : t -> Endpoint.t
  (** [peer c] is the peer of [c]. *)

  val peer_addr : t -> Unix.sockaddr
  (** [peer_addr c] is the peer address for [c]. *)

  val our_addr : t -> Unix.sockaddr
  (** [our_addr c] is our address for [c]. *)

  val socket_type : t -> Unix.socket_type
  (** [socket_type c] is the socket type of [c]. *)

  (** {1:fmt Formatting} *)

  val error : t -> string -> string
  (** [error c msg] formats an error message for connection [c]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats connections for inspection. *)

  val pp_debug : Format.formatter -> t -> unit
  (** [pp_debug] formats connection for more inspection. *)
end

val with_connection :
  ?nonblock:bool -> ?socket_type:Unix.socket_type -> peer:Endpoint.t ->
  (Connection.t -> 'a) -> ('a, string) result
(** [with_connection ~peer f] open a connection to [peer], invokes [f]
    with the connection and makes sure it is properly closed when [f]
    returns with a value or an exception. See {!Connection.open'} for
    details about the arguments. *)

val try_with_connection :
  ?nonblock:bool -> ?socket_type:Unix.socket_type -> peer:Endpoint.t ->
  (Connection.t -> 'a) -> ('a option, string) result
(** [try_with_connection] is like {!with_connection} but uses
    {!Connection.try_open} to open the connection. *)

val with_connection_close : Connection.t -> (Connection.t -> 'a) -> 'a
(** [with_connection_close c f] invokes with [c] and makes sure it
    is properly closed when [f] returns with a value or an exception. *)

(** {1:listeners Listeners} *)

(** Network connection listeners. *)
module Listener : sig

  (** {1:listeners Listeners} *)

  type t
  (** The type for connection listeners. *)

  val open' :
    ?nonblock:bool -> ?socket_type:Unix.socket_type -> ?backlog:int ->
    endpoint:Endpoint.t -> unit -> (t, string) result
  (** [open' ~endpoint ()] opens a connection listener on [endpoint] with:
      {ul
      {- [nonblock] if [true] the listening socket and accepted connection
         are in non-blocking mode (default).}
      {- [socket_type] is the type of connection. Defaults to
         {!Unix.SOCK_STREAM} or the [fd]'s type if [on] is [`Fd fd] – if
         [socket_type] is specified in this case this errors if [fd] is
         not of the specified type.}
      {-  [backlog] is the argument for {!Unix.listen} (defaults to [128]).
          Only relevant if [socket_type] is {!Unix.SOCK_STREAM}.}
      {- If a new file descriptor is created it is with [cloexec] set
         to [true].}
      {- If [peer] is [`Fd fd] it is assumed to be already bound with
         {!Unix.bind}. So the call just tries to honour the [nonblock]
         argument, gets local address from the [fd] and calls
         {!Unix.listen} if the socket type is {!Unix.SOCK_STREAM}.
         Also note that in this case {!close_noerr} does not close [fd]
         it is your duty to do so.}}

      If a listener is returned it must always eventually be closed with
      {!close_noerr}.

      {b Note.} If you used an endpoint with a port [0], {!endpoint} or
      {!endpoint_addr} have the port that has been attributed by the
      operating system. *)

  val close_noerr : t -> unit
  (** [close_noerr l] closes [l] without raising errors and can be safely
      called repeatedly. Note that if [endpoint c] is [`Fd fd] this does
      not close [fd]. *)

  (** {1:accepting Accepting connections} *)

  val accept : t -> (Connection.t, string) result
  (** [accept l] offers a connection on [l]. Blocks until a client
      connects. If a connection is returned, it must always eventually
      be closed with {!Connection.close_noerr} and its underlying
      file descriptor has [cloexec] set to [true]. *)

  (** {2:actions Actions} *)

  val wait_connection : t -> 'tag -> 'tag Action.t
  (** [wait_connection l t] is the action used by {!accept}. The action
      invocation is enabled and synchronizes with [tag] whenever {!accept}
      can be called without blocking. This is just
      {!Affect_unix.Unix.wait_readable} on the listener [fd]. *)

  (** {1:props Properties} *)

  val fd : t -> Unix.file_descr
  (** [fd l] is the listening file descriptor of [l]. *)

  val endpoint : t -> Endpoint.t
  (** [endpoint l] is the endpoint on which of [l] is listening. If you
      {!open'}ed with a port of [0] this gives you the endpoint with the port
      that was attributed by the operating system. *)

  val endpoint_addr : t -> Unix.sockaddr
  (** [endpoint_addr l] is the socket address on which [l] is listening. *)

  val socket_type  : t -> Unix.socket_type
  (** [socket_type l] is the socket type of the listener. *)

  (** {1:fmt Formatting} *)

  val error : t -> string -> string
  (** [error l msg] formats an error message for listener [l]. *)

  val pp : Format.formatter -> t -> unit
  (** [pp] formats connection listeners for inspection. *)

  val pp_debug : Format.formatter -> t -> unit
  (** [pp_debug] formats connection listeners for more inspection. *)
end

val with_listener :
  ?nonblock:bool -> ?socket_type:Unix.socket_type -> ?backlog:int ->
  on:Endpoint.t -> (Listener.t -> 'a) -> ('a, string) result
(** [with_listener ~on f] opens a listener on endpoint [on], invokes
    [f] with the listener and makes sure it is properly closed when [f]
    returns with a value or an exception. See {!Listener.open'} for
    details about the arguments. *)

(** {1:messaging Messaging} *)

(** Send and receive messages over stream sockets.

    Using simple length-value frames. *)
module Msg : sig

  (** {1:sending Sending} *)

  val send : Connection.t -> string -> (unit, string) result
  (** [send c msg] sends the bytes [msg] on [c]. The result is:
      {ul
      {- [Ok ()] if the frame for [msg] was seemingly sent successfuly.}
      {- [Error e] if the peer unexpectedly closed the connection or
         another error occurs. In this case you should proceed to
         {!Connection.close_noerr} the connection.}} *)

  val send' : Connection.t -> string -> (unit, Unix.error) result
  (** [send'] is like [send] but you get the unix error status on error,
      in case you want to try to (unreliably) reason on the peer closure. *)

  val wait_sendable : Connection.t -> 'tag -> 'tag Action.t
  (** [wait_sendable c tag] is the action used by {!send}. The action
      invocation is enabled and synchronizes with [tag] when a {!send}
      can be initiated without blocking. However since multiple
      [write(2)] may be needed, the call can still block. This is just
      {!Affect_unix.Unix.wait_writable} on the connection's file
      descriptor. *)

  (** {1:receiving Receiving} *)

  val recv : Connection.t -> (string option, string) result
  (** [recv c] receives a message from [c]. The result is:
      {ul
      {- [Ok (Some msg)] if [msg] was received in a frame.}
      {- [Ok None] if the connection was closed gracefuly ([0]
         bytes were read at the beginning of the frame).}
      {- [Error e] is the peer unexpectedly closed the connection, if a frame
         is truncated or invalid, or if any other error occurs. In this case
         you should proceed to {!Connection.close_noerr} the connection.}} *)

  val wait_recvable : Connection.t -> 'tag -> 'tag Action.t
  (** [wait_recvable c tag] is the action used by {!recv}. The action
      invocation is enabled and synchronizes with [tag] when a {!recv}
      can be initiated without blocking. However since mulitple
      [read(2)] may be needed the call can still block. This is just
      {!Affect_unix.Unix.wait_readable} on the connection's file
      descriptor. *)
end

(** {1:fmt Formatters} *)

val pp_sockaddr : Format.formatter -> Unix.sockaddr -> unit
(** [pp_sock_addr] formats socket addresses for inspection. *)

val pp_socket_type : Format.formatter -> Unix.socket_type -> unit
(** [pp_socket_type] formats socket types for inspection. *)
