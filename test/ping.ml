(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Compile and run with:

   ocamlfind ocamlopt -package affect.unix -linkpkg -o ping ping.ml
   ./ping & ./ping & ./ping

   ping tries to connect to a pong server to ping it. If none exist it
   turns into a pong server.

   Both pings and the pong server do abort fibers randomly to exercice
   a bit the abort paths. They always restart on errors, so a ping may
   become a pong server if the latter dies. *)

let ( let* ) = Result.bind

let () = Random.self_init ()
let random_ansi_color () = (if Random.bool () then 40 else 100) + Random.int 8
let random_elt l = let n = List.length l in List.nth l (Random.int n)
let random_sleep_s ~min ~max = Funix.sleep_s (min +. Random.float (max -. min))
let random_true ~pct = Random.int 100 < pct

let self_uid = Printf.sprintf "\x1b[%dm \x1b[0m" (random_ansi_color ())
let log fmt = Format.printf ("%s " ^^ fmt ^^ "\n%!") self_uid

(* Ping client *)

let random_self_abort ~pct =
  if random_true ~pct then (log "Randomly self aborting"; Fiber.self_abort ())

let ping endpoint ~max_ping_period =
  let rec ping_loop peer_uid c () = match Netmsg.send c self_uid with
  | Error _ as e -> e
  | Ok false -> Ok (`Closed peer_uid)
  | Ok true ->
      random_self_abort ~pct:2;
      match Netmsg.recv c with
      | Error _ as e -> e
      | Ok None -> Ok (`Closed peer_uid)
      | Ok (Some peer_uid) ->
          let d = Random.float max_ping_period in
          log "pong from %s next ping in %.02fs" peer_uid d;
          random_self_abort ~pct:2; Funix.sleep_s d; ping_loop peer_uid c ()
  in
  match Netmsg.connect ~endpoint with
  | Error _ as e -> e
  | Ok None -> Ok false
  | Ok Some c ->
      log "Connected to pong server";
      let finally () = Netmsg.close c in
      let* `Closed peer_uid = Fun.protect ~finally (ping_loop "" c) in
      log "Connection closed by pong %s" peer_uid;
      Ok true

(* Pong server *)

let pong c () =
  let plog peer_uid fmt = log ("ping      %s " ^^ fmt) peer_uid in
  let handle_abort peer_uid = plog peer_uid "aborting"; raise Fiber.Abort in
  let peer_closed peer_uid = plog peer_uid "closed" in
  let peer_error peer_uid e = plog peer_uid "error: %s" e in
  let relax () = Funix.sleep_s (Random.float 0.3) in
  let rec pong_loop peer_uid c = match Netmsg.recv c with
  | exception Fiber.Abort -> handle_abort peer_uid
  | Error msg -> peer_error peer_uid msg
  | Ok None -> peer_closed peer_uid
  | Ok Some peer_uid ->
      log "ping from %s" peer_uid;
      match relax (); Netmsg.send c self_uid with
      | exception Fiber.Abort -> handle_abort peer_uid
      | Error msg -> peer_error peer_uid msg
      | Ok false -> peer_closed peer_uid
      | Ok true -> pong_loop peer_uid c
  in
  pong_loop "?" c

let pong_server endpoint =
  let reap_terminated f = match Fiber.poll f with
  | None -> Some f | Some _ -> None
  in
  let rec random_abort fs () =
    random_sleep_s ~min:3. ~max:5.;
    if fs = [] || random_true ~pct:75 then random_abort fs () else
    (log "Randomly aborting a fiber"; Fiber.abort (random_elt fs))
  in
  let rec server_loop peers l () =
    let peers = List.filter_map reap_terminated peers in
    let random_abort = Fiber.spawn (random_abort peers) in
    let new_ping = Fiber.spawn (fun () -> Netmsg.listen l) in
    match Fiber.either random_abort new_ping with
    | Some (Either.Left ()) -> server_loop peers l ()
    | Some (Either.Right (Ok c)) ->
        log "New ping!";
        let finally () = Netmsg.close c in
        let peers = Fiber.spawn ~finally (pong c) :: peers in
        server_loop peers l ()
    | Some (Either.Right (Error _ as e)) -> e
    | None -> Error "Server internal error"
  in
  match Netmsg.listener ~endpoint () with
  | Error _ as e -> e
  | Ok l ->
      log "Waiting for pings…";
      let finally () = Netmsg.close_listener l in
      Fun.protect ~finally (server_loop [] l)

(* Ping pong *)

let rec ping_pong endpoint ~max_ping_period () =
  Result.join @@ Funix.Signal.with' Sys.sigpipe Sys.Signal_ignore @@ fun () ->
  log "Contacting pong server on %a" Funix.pp_endpoint endpoint;
  match ping endpoint ~max_ping_period with
  | Error _ as e -> e
  | Ok true -> ping_pong endpoint ~max_ping_period ()
  | Ok false ->
      log "Could not connect, will pong instead.";
      pong_server endpoint

let rec main () =
  let endpoint = `Host ("localhost", 10101) in
  let max_ping_period = 1.5 in
  let ping_pong = ping_pong endpoint ~max_ping_period in
  match Fiber.run ~unblock:Funix.unblock ping_pong with
  | None -> main ()
  | Some (Error e) -> main ()
  | Some (Ok ()) -> exit 0

let () = if !Sys.interactive then () else main ()
