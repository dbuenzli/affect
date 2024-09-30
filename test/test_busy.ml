(*---------------------------------------------------------------------------
   Copyright (c) 2024 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Tests that a blocked fiber does not busy wait.

   b0 -- test_busy
   top -pid $(pgrep test_busy)
   nc localhost 10101 *)

let ( let* ) = Result.bind
let log fmt = Format.printf (fmt ^^ "@.")

let serve_client ~endpoint =
  Result.join @@ Funix.Signal.with' Sys.sigpipe Sys.Signal_ignore @@ fun () ->
  let* l = Netmsg.listener ~endpoint () in
  let finally () = Netmsg.close_listener l in
  Fun.protect ~finally @@ fun () ->
  log "Waiting for a client…";
  let* conn = Netmsg.listen l in
  log "Got a client! But we have nothing to say, closing.";
  Netmsg.close conn;
  Ok ()

let rec main () =
  Fiber.main ~unblock:Funix.unblock @@ fun () ->
  let endpoint = `Host ("localhost", 10101) in
  let serve = Fiber.async @@ fun () -> serve_client ~endpoint in
  match Fiber.await serve (* This must not busy wait *) with
  | Ok () -> 0
  | Error e -> log "Error: %s" e; 1

let () = if !Sys.interactive then () else exit (main ())
