(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

let fmt_error fmt = Format.kasprintf (fun s -> Error s) fmt
let uerror = Unix.error_message
let connection_error ep e = fmt_error "connection %a: %s" Funix.pp_endpoint ep e
let listener_error ep e = fmt_error "listener %a: %s" Funix.pp_endpoint ep e

(* Connection *)

type t =
  { ep : Funix.endpoint;
    fd : Unix.file_descr;
    len_buf : Bytes.t; (* 8 bytes for encoding an int64 *)
    close : bool (* true if [fd] must be closed at the end. *); }

let make ep fd ~close = { ep; fd; len_buf = Bytes.create 8; close  }

let connect ~endpoint:ep =
  match Funix.socket_of_endpoint ep Unix.SOCK_STREAM with
  | Error e -> connection_error ep e
  | Ok (None, sock, close) -> Ok (Some (make ep sock ~close))
  | Ok (Some addr, sock, close) ->
      match Funix.connect sock addr with
      | () -> Ok (Some (make ep sock ~close))
      | exception exn ->
          let bt = Printexc.get_raw_backtrace () in
          if close then Funix.close_noerr sock;
          match exn with
          | Unix.(Unix_error ((ENOENT | ECONNREFUSED), _, _)) -> Ok None
          | Unix.Unix_error (e,_,_) -> connection_error ep (uerror e)
          | exn -> Printexc.raise_with_backtrace exn bt

let close c =
  if c.close then begin
    (try Unix.shutdown c.fd Unix.SHUTDOWN_ALL with Unix.Unix_error _ -> ());
    Funix.close_noerr c.fd
  end

let fd c = c.fd

(* Communication *)

let send c s =
  try
    let len = String.length s in
    Bytes.set_int64_be c.len_buf 0 (Int64.of_int len);
    Funix.write c.fd c.len_buf ~start:0 ~len:(Bytes.length c.len_buf);
    Funix.write c.fd (Bytes.unsafe_of_string s) ~start:0 ~len;
    Ok true
  with
  | Unix.(Unix_error (EPIPE, _, _)) -> Ok false
  | Unix.Unix_error (e, _, _) -> connection_error c.ep (Unix.error_message e)

let recv c =
  try
    let len = Bytes.length c.len_buf in
    match Funix.read c.fd c.len_buf ~start:0 ~len with
    | false -> Ok None
    | true ->
        let len = Int64.to_int (Bytes.get_int64_be c.len_buf 0) in
        let b = Bytes.create len in
        match Funix.read c.fd b ~start:0 ~len with
        | true -> Ok (Some (Bytes.unsafe_to_string b))
        | false -> connection_error c.ep "Unexpected end of connection."
  with
  | Unix.Unix_error (e, _, _) -> connection_error c.ep (Unix.error_message e)

(* Listener *)

let unlink_noerr file = try Unix.unlink file with Unix.Unix_error _ -> ()
let cleanup_sockaddr_noerr addr = match addr with
| Some Unix.ADDR_UNIX file -> unlink_noerr file  | _ -> ()

type listener =
  { ep : Funix.endpoint;
    fd : Unix.file_descr;
    addr : Unix.sockaddr option;
    close : bool (* true if [fd] must be closed at the end. *) }

let close_listener l =
  cleanup_sockaddr_noerr l.addr;
  if l.close then Funix.close_noerr l.fd

let listener ?(backlog = 128) ~endpoint:ep () =
  match Funix.socket_of_endpoint ep Unix.SOCK_STREAM with
  | Error e -> connection_error ep e
  | Ok (addr, sock, close) ->
      let l = { ep; fd = sock; addr; close } in
      try
        begin match addr with
        | None -> ()
        | Some (Unix.ADDR_UNIX _ as addr) -> Unix.bind sock addr
        | Some (Unix.ADDR_INET _ as addr) ->
            Unix.setsockopt sock Unix.SO_REUSEADDR true;
            Unix.bind sock addr
        end;
        Unix.listen sock backlog;
        Ok l
      with
      | exn ->
          let bt = Printexc.get_raw_backtrace () in
          close_listener l;
          match exn with
          | Unix.Unix_error (e, _, _) ->
              listener_error ep (Unix.error_message e)
          | exn -> Printexc.raise_with_backtrace exn bt

let listen l = match Funix.accept l.fd with
| fd, addr -> Ok (make (`Sockaddr addr) fd ~close:true)
| exception Unix.Unix_error (e, _, _) ->
    listener_error l.ep (Unix.error_message e)

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
