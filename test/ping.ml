(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* Compile and run with:

   ocamlfind ocamlopt -package affect.unix -linkpkg -o ping ping.ml
   ./ping & ./ping & ./ping

   ping tries to connect to a pong server to ping it. If none exist it
   turns into a pong server.

   Both pings and the pong server do cancel fibers randomly to exercice
   a bit the cancel paths. They always restart on errors, so a ping may
   become a pong server if the latter dies. *)

let ( let* ) = Result.bind

let () = Random.self_init ()
let random_ansi_color () = (if Random.bool () then 40 else 100) + Random.int 8
let random_elt l = let n = List.length l in List.nth l (Random.int n)
let random_sleep_s ~min ~max = Funix.sleep_s (min +. Random.float (max -. min))
let random_true ~pct = Random.int 101 <= pct

let this_peer_uid = Printf.sprintf "\x1b[%dm \x1b[0m" (random_ansi_color ())
let log fmt = Format.printf ("%s " ^^ fmt ^^ "@.") this_peer_uid

(* Ping client *)

let random_self_cancel ~pct =
  if not (random_true ~pct) then () else
  (log "Randomly self cancelling"; Fiber.self_cancel (); raise Fiber.Cancelled)

let ping_self_cancel_pct = 2

let ping endpoint ~max_ping_period =
  let closed ~peer_uid = log "          %s closed by pong server" peer_uid in
  let rec ping_loop ~peer_uid peer =
    let* sent = Netmsg.send peer this_peer_uid in
    if not sent then Ok (closed ~peer_uid) else
    let () = random_self_cancel ~pct:ping_self_cancel_pct in
    let* msg = Netmsg.recv peer in
    match msg with
    | None -> Ok (closed ~peer_uid)
    | Some peer_uid ->
        let () = random_self_cancel ~pct:ping_self_cancel_pct in
        let d = Random.float max_ping_period in
        log "pong from %s next ping in %.02fs" peer_uid d;
        Funix.sleep_s d;
        ping_loop ~peer_uid peer
  in
  let* peer = Netmsg.connect ~endpoint in
  match peer with
  | None -> Ok false (* did not ping *)
  | Some peer ->
      let finally () = Netmsg.close peer in
      Fun.protect ~finally @@ fun () ->
      log "Connected to pong server";
      let* () = try ping_loop ~peer_uid:"" (* none *) peer with
      | Fiber.Cancelled -> Ok ()
      in
      Ok true (* did ping *)

(* Pong server *)

let pong peer =
  let finally () = Netmsg.close peer in
  Fun.protect ~finally @@ fun () ->
  let plog peer_uid fmt = log ("ping      %s " ^^ fmt) peer_uid in
  let peer_closed ~peer_uid = plog peer_uid "closed" in
  let peer_error ~peer_uid e = plog peer_uid "error: %s" e in
  let jitter_response () = Funix.sleep_s (Random.float 0.3) in
  let rec pong_loop ~peer_uid peer =
    Fiber.self_check_cancellation ();
    match Netmsg.recv peer with
    | Error e -> peer_error ~peer_uid e
    | Ok None -> peer_closed ~peer_uid
    | Ok Some peer_uid ->
        log "ping from %s" peer_uid;
        jitter_response ();
        match Netmsg.send peer this_peer_uid with
        | Error e -> peer_error ~peer_uid e
        | Ok false -> peer_closed ~peer_uid
        | Ok true -> pong_loop ~peer_uid peer
  in
  try pong_loop ~peer_uid:"?" peer with
  | Fiber.Cancelled -> ()

let pong_server endpoint =
  let reap_terminated f = match Fiber.poll f with
  | None -> Some f | Some _ -> None
  in
  let rec random_close_peer peers =
    (* if fs = [] then Fiber.await_cancelled ();  TODO *)
    random_sleep_s ~min:3. ~max:5.;
    if peers = [] || random_true ~pct:40
    then random_close_peer peers else
    (log "Randomly closing a peer"; Fiber.cancel (random_elt peers))
  in
  let new_listen l =
    let* peer = Netmsg.listen l in
    if Fiber.self_is_cancelled ()
    then (Netmsg.close peer; raise Fiber.Cancelled)
    else Ok peer
  in
  let rec server_loop l peers =
    let peers = List.filter_map reap_terminated peers in
    let random_abort = Fiber.async @@ fun () -> random_close_peer peers in
    let new_ping = Fiber.async @@ fun () -> new_listen l in
    match Fiber.pick_either random_abort new_ping with
    | exception Fiber.Cancelled -> server_loop l peers
    | Either.Left () -> server_loop l peers
    | Either.Right (Error _ as e) -> e
    | Either.Right (Ok peer) ->
        log "New ping";
        server_loop l (Fiber.async (fun () -> pong peer) :: peers)
  in
  let* l = Netmsg.listener ~endpoint () in
  let finally () = Netmsg.close_listener l in
  Fun.protect ~finally @@ fun () ->
  log "Waiting for pings…";
  server_loop l []

(* Ping pong *)

let rec ping_or_pong endpoint ~max_ping_period =
  Result.join @@ Funix.Signal.with' Sys.sigpipe Sys.Signal_ignore @@ fun () ->
  log "Contacting pong server on %a" Funix.pp_endpoint endpoint;
  let* did_ping = ping endpoint ~max_ping_period in
  if did_ping then Ok () else
  (log "Could not connect, will pong instead."; pong_server endpoint)

let main () =
  Fiber.main ~unblock:Funix.unblock @@ fun () ->
  let endpoint = `Host ("localhost", 10101) in
  let relax () = random_sleep_s ~min:0.1 ~max:0.5 in
  let rec loop () = match ping_or_pong endpoint ~max_ping_period:1.5 with
  | Ok () -> relax (); loop ()
  | Error e -> log "Error: %s" e; relax (); loop ()
  in
  loop ()

let () = if !Sys.interactive then () else exit (main ())
