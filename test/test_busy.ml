(*---------------------------------------------------------------------------
   Copyright (c) 2024 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let ( let* ) = Result.bind

let log fmt = Printf.printf (fmt ^^ "%!")

let serve_client ~endpoint () =
  Result.join @@ Funix.Signal.with' Sys.sigpipe Sys.Signal_ignore @@ fun () ->
  let* l = Netmsg.listener ~endpoint () in
  log "Waiting for a client…";
  let finally () = Netmsg.close_listener l in
  Fun.protect ~finally @@ fun () ->
  let* conn = Netmsg.listen l in
  log "Got a client! But we have nothing to say, closing.";
  Netmsg.close conn;
  Ok ()

let serve_client_wrap ~endpoint () =
  let f = Fiber.spawn (serve_client ~endpoint) in
  match Fiber.join f with (* Makes the whole thing BUSY ! *)
  | None -> Error "Aborted!!!"
  | Some v -> v

let rec main () =
  let endpoint = `Host ("localhost", 110011) in
  let serve_client = serve_client_wrap ~endpoint in
  match Fiber.run ~unblock:Funix.unblock serve_client with
  | None -> main ()
  | Some (Error e) -> Printf.eprintf "Error: %s" e; exit 1
  | Some (Ok ()) -> exit 0

let () = if !Sys.interactive then () else main ()
