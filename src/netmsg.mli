(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Send messages over stream sockets. *)

(** {1:connection Connection} *)

type t
(** The type for connections. *)

val connect : endpoint:Funix.endpoint -> (t option, string) result
(** [connect ~endpoint] connects to a server offered on endpoint
    [endpoint]. [None] is returned if no server could be found. *)

val close : t -> unit
(** [close c] closes a connection. This never errors. *)

val fd : t -> Unix.file_descr
(** [fd c] is the file descriptor of [c]. *)

(** {2:communication Communication} *)

val send : t -> string -> (bool, string) result
(** [send c s] sends bytes [s] on [c]. The result is [Ok false] if
    the peer ends the connection. *)

val recv : t -> (string option, string) result
(** [recv c] receives bytes from [c] and is [Ok None] if the peer
    ends the connection. *)

(** {1:listener Connection listeners} *)

type listener
(** The type for connection listeners. *)

val listener :
  ?backlog:int -> endpoint:Funix.endpoint -> unit -> (listener, string) result
(** [listener ~backlog ~endpoint ()] is a connection listener on [endpoint].
    [backlog] is the argument for {!Unix.listen} (defaults to [128]). *)

val listen : listener -> (t, string) result
(** [listen ~endpoint] offers a connection on [endpoint].  Blocks
    until a client connects. The caller must eventually {!close} the
    connection. *)

val close_listener : listener -> unit
(** [close_listener l] closes listener [l].  *)
