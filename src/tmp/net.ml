(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Result.Syntax
open Affect_unix

(* Both in Connection.t and Listener.t we have a flag for closure to
   avoid close(2)ing multiple times. Due to the POSIX [0] strategy of
   fd reallocation, reuse happens quickly, especially in parallel setting,
   so multiple close may end up closing newly allocated fd which is puzzling.

   [0]: https://pubs.opengroup.org/onlinepubs/9799919799/\
        functions/V2_chap02.html#tag_16_06 *)

(* XXX add support for {RCV,SND}TIMEO. *)

(* Formatting *)

let strf = Format.asprintf
let pf = Format.fprintf
let pp_name_port ppf (n, p) = pf ppf "%s:%d" n p
let pp_sockaddr ppf = function
| Unix.ADDR_UNIX s -> Format.pp_print_string ppf s
| Unix.ADDR_INET (a, p) -> pp_name_port ppf (Unix.string_of_inet_addr a, p)

let pp_sockaddr_no_port ppf = function
| Unix.ADDR_UNIX s -> Format.pp_print_string ppf s
| Unix.ADDR_INET (a, _) -> pf ppf "%s" (Unix.string_of_inet_addr a)

let pp_socket_type ppf = function
| Unix.SOCK_STREAM -> Format.pp_print_string ppf "tcp"
| Unix.SOCK_DGRAM -> Format.pp_print_string ppf "udp"
| Unix.SOCK_RAW -> Format.pp_print_string ppf "raw"
| Unix.SOCK_SEQPACKET -> Format.pp_print_string ppf "seq"

(* Unix helpers *)

let uerror = Unix.error_message

let rec connect fd addr = try Unix.connect fd addr with
| Unix.Unix_error (EINTR, _, _) -> connect fd addr

let rec accept fd = try Unix.accept ~cloexec:true fd with
| Unix.Unix_error (EINTR, _, _) -> accept fd

let set_nonblock ~nonblock fd =
  if nonblock then Unix.set_nonblock fd else Unix.clear_nonblock fd

let get_socket_type fd = (* XXX unclear how portable that is *)
  match Unix.getsockopt_int fd Unix.SO_TYPE with
  | 1 -> Ok Unix.SOCK_STREAM
  | 2 -> Ok Unix.SOCK_DGRAM
  | 3 -> Ok Unix.SOCK_RAW
  | 4 -> Ok Unix.SOCK_SEQPACKET
  | n -> Error (strf "Unknown socket type: %d" n)
  | exception Unix.Unix_error (ENOTSOCK, _, _) ->
      Error "Unknown socket type: not a socket"

(* Endpoints *)

module Endpoint = struct
  type t =
  [ `Host of string * int
  | `Sockaddr of Unix.sockaddr
  | `Fd of Unix.file_descr ]

  let of_string ~default_port s =
    if String.exists (fun c -> c = '/' || c = '\\') s
    then Ok (`Sockaddr (Unix.ADDR_UNIX s)) else
    match String.rindex_opt s ':' with
    | None -> Ok (`Host (s, default_port))
    | Some i ->
        match String.index_from_opt s i ']' with (* beware IPv6 *)
        | Some _ -> Ok (`Host (s, default_port))
        | None ->
            let len = String.length s in
            let h = String.sub s 0 i in
            let p =
              if i + 1 > len then "" else
              String.sub s (i + 1) (len - i - 1)
            in
            match int_of_string_opt p with
            | None -> Error (strf "port %S not an integer" p)
            | Some p -> Ok (`Host (h, p))

  let with_port_of_sockaddr sockaddr ep = match sockaddr with
  | Unix.ADDR_UNIX _ -> ep
  | Unix.ADDR_INET (_, port) ->
      match ep with
      | `Host (n, _) -> `Host (n, port)
      | `Sockaddr (Unix.ADDR_INET (a, _)) -> `Sockaddr (ADDR_INET (a, port))
      | ep -> ep

  let pp ppf ep = match ep with
  | `Host (n, p) -> pp_name_port ppf (n, p)
  | `Sockaddr sockaddr -> pp_sockaddr ppf sockaddr
  | `Fd fd -> Format.pp_print_string ppf "<fd>"

  let pp_with_addr ppf (ep, ep_addr) = match ep with
  | `Sockaddr sockaddr -> pp_sockaddr ppf sockaddr
  | `Fd _ -> pp_sockaddr ppf ep_addr
  | `Host (n, port) -> pf ppf "%s[%a]:%d" n pp_sockaddr_no_port ep_addr port
end

let socket ?(nonblock = true) endpoint socket_type =
  let* addr = match endpoint with  (* assert (endpoint <> Fd _) *)
  | `Fd _ -> assert false
  | `Sockaddr addr -> Ok addr
  | `Host (name, port) ->
      (* XXX we have a notorious blocker here… *)
      match Unix.gethostbyname name with
      | exception Not_found -> Error (strf "%s: Host not found" name)
      | h -> Ok (Unix.ADDR_INET (h.h_addr_list.(0), port))
  in
  let domain = Unix.domain_of_sockaddr addr in
  match Unix.socket ~cloexec:true domain socket_type 0 with
  | exception Unix.Unix_error (e, _, _) -> Error (uerror e)
  | fd -> Ok (addr, fd)

(* Connections *)

module Connection = struct
  type t =
    { peer : Endpoint.t;
      peer_addr : Unix.sockaddr;
      our_addr : Unix.sockaddr;
      socket_type : Unix.socket_type;
      fd : Unix.file_descr;
      closed : bool Atomic.t; }

  let error' peer e = strf "↗%a %s" Endpoint.pp peer e
  let error c e = error' c.peer e
  let err_no_connection ep = error' ep "Not reachable or refused"
  let err_fd_socket_type_mismatch ~fd ct =
    strf "fd endpoint socket type (%a) mismatches connection type (%a)"
      pp_socket_type fd pp_socket_type ct

  let open_fd_peer_exn ~nonblock ?socket_type ~peer fd =
    let* fd_socket_type = get_socket_type fd in
    let* socket_type = match socket_type with
    | None -> Ok fd_socket_type
    | Some socket_type ->
        if socket_type = fd_socket_type then Ok socket_type else
        Error (err_fd_socket_type_mismatch ~fd:fd_socket_type socket_type);
    in
    set_nonblock ~nonblock fd;
    let peer_addr = Unix.getpeername fd in
    let our_addr = Unix.getsockname fd in
    let closed = Atomic.make false in
    Ok { peer; peer_addr; our_addr; socket_type; fd; closed }

  let open_exn ?(nonblock = true) ?socket_type ~peer () = match peer with
  | `Fd fd -> open_fd_peer_exn ~nonblock ?socket_type ~peer fd
  | peer ->
      let socket_type = Option.value ~default:Unix.SOCK_STREAM socket_type in
      let* addr, fd = socket ~nonblock peer socket_type in
      try
        connect fd addr;
        let peer_addr = Unix.getpeername fd in
        let our_addr = Unix.getsockname fd in
        let closed = Atomic.make false in
        Ok { peer; peer_addr; our_addr; socket_type; fd; closed }
      with exn ->
        let bt = Printexc.get_raw_backtrace () in
        Unix.close_noerr fd;
        Printexc.raise_with_backtrace exn bt

  let open' ?nonblock ?socket_type ~peer () =
    match open_exn ?nonblock ?socket_type ~peer () with
    | Ok _ as v -> v
    | Error e -> Error (error' peer e)
    | exception Unix.Unix_error (e,_,_) -> Error (error' peer (uerror e))

  let try_open ?nonblock ?socket_type ~peer () =
    match open_exn ?nonblock ?socket_type ~peer () with
    | Ok v -> Ok (Some v)
    | Error e -> Error (error' peer e)
    | exception Unix.Unix_error (e, func, _) ->
        match e with
        | ENOENT | ECONNREFUSED | EHOSTUNREACH -> Ok None
        | e -> Error (error' peer (uerror e))

  let of_accept_fd_exn ~nonblock ~peer_addr socket_type fd =
    set_nonblock ~nonblock fd;
    let peer = `Sockaddr peer_addr in
    let our_addr = Unix.getsockname fd in
    let closed = Atomic.make false in
    { peer; peer_addr; our_addr; socket_type; fd; closed }

  let close_noerr c =
    if Atomic.exchange c.closed true then () else
    (match c.peer with `Fd _ -> () | _ -> Unix.close_noerr c.fd)

  let fd c = c.fd
  let peer c = c.peer
  let peer_addr c = c.peer_addr
  let our_addr c = c.our_addr
  let socket_type c = c.socket_type
  let pp ppf c = pf ppf "↗%a" Endpoint.pp_with_addr (c.peer, c.peer_addr)
  let pp_debug ppf c =
    pf ppf "%a %a ↘%a" pp_socket_type c.socket_type pp c pp_sockaddr c.our_addr
end

let with_connection ?nonblock ?socket_type ~peer f =
  let* c = Connection.open' ?nonblock ?socket_type ~peer () in
  let finally () = Connection.close_noerr c in
  Ok (Fun.protect ~finally (fun () -> f c))

let try_with_connection ?nonblock ?socket_type ~peer f =
  let* c = Connection.try_open ?nonblock ?socket_type ~peer () in
  match c with
  | None -> Ok None
  | Some c ->
      let finally () = Connection.close_noerr c in
      Ok (Some (Fun.protect ~finally (fun () -> f c)))

let with_connection_close c f =
  let finally () = Connection.close_noerr c in
  Fun.protect ~finally (fun () -> f c)

(* listeners *)

module Listener = struct
  type t =
    { endpoint : Endpoint.t;
      endpoint_addr : Unix.sockaddr;
      socket_type : Unix.socket_type;
      nonblock : bool;
      fd : Unix.file_descr;
      closed : bool Atomic.t; }

  let error' on e = strf "↘%a %s" Endpoint.pp on e
  let error l e = error' l.endpoint e
  let err_fd_socket_type_mismatch ~fd lt =
    strf "fd endpoint socket type (%a) mismatches listener type (%a)"
      pp_socket_type fd pp_socket_type lt

  let close_noerr_addr addr fd =
    begin match addr with
    | Unix.ADDR_UNIX file -> (try Unix.unlink file with Unix.Unix_error _ -> ())
    | _ -> ()
    end;
    Unix.close_noerr fd

  let close_noerr l =
    if Atomic.exchange l.closed true then () else
    close_noerr_addr l.endpoint_addr l.fd

  let open_fd_peer_exn ~nonblock ?socket_type ~backlog ~endpoint fd =
    let* fd_socket_type = get_socket_type fd in
    let* socket_type = match socket_type with
    | None -> Ok fd_socket_type
    | Some socket_type ->
        if socket_type = fd_socket_type then Ok socket_type else
        Error (err_fd_socket_type_mismatch ~fd:fd_socket_type socket_type);
    in
    set_nonblock ~nonblock fd;
    if socket_type = Unix.SOCK_STREAM then Unix.listen fd backlog;
    let endpoint_addr = Unix.getsockname fd in
    let closed = Atomic.make false in
    Ok { endpoint; endpoint_addr; socket_type; nonblock; fd; closed }

  let open_exn ?(nonblock = true) ?socket_type ?(backlog = 128) ~endpoint () =
    match endpoint with
    | `Fd fd -> open_fd_peer_exn ~nonblock ?socket_type ~backlog ~endpoint fd
    | endpoint ->
        let socket_type = Option.value ~default:Unix.SOCK_STREAM socket_type in
        let* addr, fd = socket ~nonblock endpoint socket_type in
        try
          begin match addr with
          | Unix.ADDR_INET _ -> Unix.setsockopt fd Unix.SO_REUSEADDR true
          | _ -> ()
          end;
          Unix.bind fd addr;
          if socket_type = Unix.SOCK_STREAM then Unix.listen fd backlog;
          let addr = Unix.getsockname fd in
          let endpoint = Endpoint.with_port_of_sockaddr addr endpoint in
          let closed = Atomic.make false in
          Ok { endpoint; endpoint_addr = addr; socket_type; nonblock; fd;
               closed }
        with
        | exn ->
            let bt = Printexc.get_raw_backtrace () in
            close_noerr_addr addr fd;
            match exn with
            | Unix.Unix_error (e, _, _) -> Error (error' endpoint (uerror e))
            | exn -> Printexc.raise_with_backtrace exn bt

  let open' ?nonblock ?socket_type ?backlog ~endpoint () =
    match open_exn ?nonblock ?socket_type ?backlog ~endpoint () with
    | exception Unix.Unix_error (e,_,_) -> Error (error' endpoint (uerror e))
    | Error e -> Error (error' endpoint e)
    | Ok _ as v -> v

  let accept l =
    try
      let fd, peer_addr = accept l.fd in
      let nonblock = l.nonblock in
      Ok (Connection.of_accept_fd_exn ~nonblock ~peer_addr l.socket_type fd)
    with
    | Unix.Unix_error (e,_,_) -> Error (error l (uerror e))

  let wait_connection l tag = Unix.wait_readable l.fd tag
  let fd l = l.fd
  let endpoint l = l.endpoint
  let endpoint_addr l = l.endpoint_addr
  let socket_type l = l.socket_type
  let pp ppf l = pf ppf "↘%a" Endpoint.pp_with_addr (l.endpoint,l.endpoint_addr)
  let pp_debug ppf l = pf ppf "%a %a" pp_socket_type l.socket_type pp l
end

let with_listener ?nonblock ?socket_type ?backlog ~on:endpoint f =
  let* l = Listener.open' ?nonblock ?socket_type ?backlog ~endpoint () in
  let finally () = Listener.close_noerr l in
  Ok (Fun.protect ~finally (fun () -> f l))

(* Messaging *)

module Msg = struct
  let data_len_size = 8

  let connection_error c e = Connection.error c (uerror e)

  (* Sending *)

  let rec write fd b first len = match Unix.single_write fd b first len with
  | exception Unix.Unix_error (EINTR, _, _) -> write fd b first len
  | count when count < len -> write fd b (first + count) (len - count)
  | _ -> ()

  let write_data_len c data_len =
    let b = Bytes.create data_len_size in
    Bytes.set_int64_be b 0 (Int64.of_int data_len);
    write (Connection.fd c) b 0 data_len_size

  let write_data c msg =
    write (Connection.fd c) (Bytes.unsafe_of_string msg) 0 (String.length msg)

  let send' c msg =
    try write_data_len c (String.length msg); write_data c msg; Ok () with
    | Unix.Unix_error (e, _, _) -> Error e

  let send c msg = Result.map_error (connection_error c) (send' c msg)
  let wait_sendable c tag = Unix.wait_writable (Connection.fd c) tag

  (* Receiving *)

  let err_len c len = Connection.error c (strf "Illegal message length %d" len)
  let err_eof c = Connection.error c "Unexpected end of connection"

  let rec read_no_eintr fd b first len = try Unix.read fd b first len with
  | Unix.Unix_error (EINTR, _, _) -> read_no_eintr fd b first len

  let read_data_len c =
    let rec read fd b first len = match read_no_eintr fd b first len with
    | 0 when first = 0 -> Ok None
    | 0 when len > 0 -> Error (err_eof c)
    | count when count < len -> read fd b (first + count) (len - count)
    | _ ->
        let data_len = Int64.to_int (Bytes.get_int64_be b 0) in
        if data_len >= 0 then Ok (Some data_len) else Error (err_len c data_len)
    in
    read (Connection.fd c) (Bytes.create data_len_size) 0 data_len_size

  let read_data c data_len =
    let rec read fd b first len = match read_no_eintr fd b first len with
    | 0 when len > 0 -> Error (err_eof c)
    | count when count < len -> read fd b (first + count) (len - count)
    | _ -> Ok (Some (Bytes.unsafe_to_string b))
    in
    read (Connection.fd c) (Bytes.create data_len) 0 data_len

  let recv c =
    try match read_data_len c with
    | Error _ as e -> e
    | Ok None as r -> r
    | Ok (Some data_len) -> read_data c data_len
    with
    | Unix.Unix_error (e, _, _) -> Error (connection_error c e)

  let wait_recvable c tag = Unix.wait_readable (Connection.fd c) tag
end
