(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* ping tries to connect to a pong server to ping it. If none exist it
   turns into a pong server.

   The program does so forever restarting over on errors or cancellations.
   So a ping client may eventually become the pong server if the latter dies.

   Both ping clients and the pong server cancel asynchronous functions randomly
   to exercice the cancel paths and show how the model fares in practice. *)

open Result.Syntax
open Affect
open Affect_unix

let closed_str = "\x1b[31mclosed\x1b[0m"
let cancelling_str = "\x1b[31mcancelling\x1b[0m"
let error_str = "\x1b[31Error\x1b[0m"

let () = Random.self_init ()
let random_ansi_color () = (if Random.bool () then 40 else 100) + Random.int 8
let random_elt l = let n = List.length l in List.nth l (Random.int n)
let random_span_ms ?(min = 0) ~max () =
  let d = Random.int_in_range ~min ~max in
  Mtime.Span.(d * ms)

let this_peer_uid = Printf.sprintf "\x1b[%dm \x1b[0m" (random_ansi_color ())
let log fmt = Format.printf ("%s " ^^ fmt ^^ "@.") this_peer_uid

let random_cancel_current ~pct () =
  if Random.int_in_range ~min:0 ~max:100 <= pct
  then (log "Randomly %s" cancelling_str; Fun.Async.cancel_current ())

(* Ping client *)

let ping ~peer ~max_ping_period_ms =
  let random_cancel_current = random_cancel_current ~pct:2 in
  let closed ~peer_uid =
    log "          %s %s by pong server" peer_uid closed_str
  in
  Fun.Async.call @@ fun () ->
  let run c =
    let rec loop ~peer_uid c = match Net.Msg.send' c this_peer_uid with
    | Error EPIPE -> Ok (closed ~peer_uid)
    | Error e -> Error (Net.Connection.error c (Unix.error_message e))
    | Ok () ->
        random_cancel_current ();
        match Net.Msg.recv c with
        | Error _ as e -> e
        | Ok None -> Ok (closed ~peer_uid)
        | Ok Some peer_uid ->
            random_cancel_current ();
            let d = random_span_ms ~max:max_ping_period_ms () in
            log "pong from %s next ping in %a" peer_uid Mtime.Span.pp d;
            Mtime.wait_for d;
            loop ~peer_uid c
    in
    log "Connected to pong server %a" Net.Connection.pp c;
    match loop ~peer_uid:"" (* none *) c with
    | Error _ as e -> e
    | Ok () -> Ok true
    | exception Fun.Async.Cancelled -> Ok true
  in
  match Net.Connection.try_open ~peer () with
  | Error _ as e -> e
  | Ok None -> Ok false (* did not ping *)
  | Ok (Some c) -> Net.with_connection_close c run

(* Pong server *)

let handle_ping_client c =
  let plog peer_uid fmt = log ("ping      %s " ^^ fmt) peer_uid in
  let peer_closed ~peer_uid = plog peer_uid "%s" closed_str in
  let peer_error ~peer_uid e = plog peer_uid "error: %s" e in
  let jitter_response () = Mtime.wait_for (random_span_ms ~max:200 ()) in
  Fun.Async.call @@ fun () ->
  Net.with_connection_close c @@ fun c ->
  log "Connection from ping client %a" Net.Connection.pp c;
  let rec loop ~peer_uid c = match Net.Msg.recv c with
  | Error e -> peer_error ~peer_uid e
  | Ok None -> peer_closed ~peer_uid
  | Ok Some peer_uid ->
      log "ping from %s" peer_uid;
      jitter_response ();
      match Net.Msg.send' c this_peer_uid with
      | Error EPIPE -> peer_closed ~peer_uid
      | Error e -> peer_error ~peer_uid (Unix.error_message e)
      | Ok () -> loop ~peer_uid c
  in
  try loop ~peer_uid:"?" c with
  | Fun.Async.Cancelled -> ()
  | exn -> Fun.Async.trap_exn exn (* We don't [get] the call so witness exns *)

let serve l new_client =
  (* [new_client] is only used to advertise new clients to our [chaos] maker. *)
  Fun.Async.call @@ fun () ->
  let rec loop l new_client = match Net.Listener.accept l with
  | Error _ as e -> e
  | Ok c ->
    let client = handle_ping_client c in
    Cell.Drop.put new_client (client, c); loop l new_client
  in
  try loop l new_client with Fun.Async.Cancelled -> Ok ()

let client_is_live (handler, c) = not (Fun.Async.has_returned handler)

let try_chaos serve clients = match Random.int_in_range ~min:0 ~max:100 with
| n when n <= 10 ->
    log "Chaos %s server" cancelling_str;
    Fun.Async.cancel serve
| n when n <= 40 && clients <> [] ->
    let oh_no, c as client = random_elt clients in
    if client_is_live client then begin
      log "Chaos %s %a" cancelling_str Net.Connection.pp c;
      Fun.Async.cancel oh_no
    end
| _ -> ()

let chaos serve new_client =
  (* This supervises and chaoticises the [serve] loop which reports new clients
     on [new_client] via a drop cell. *)
  let time_to_next_chaos last_chaos =
    Mtime.Span.((3 * s) - (Mtime.count last_chaos))
  in
  let rec loop serve new_client clients last_chaos =
    let next_chaos = time_to_next_chaos last_chaos in
    let next_step =
      let stop = Action.map (fun r -> `Stop r) (Fun.Async.get' serve) in
      let newc = Action.map (fun c -> `New c) (Cell.Drop.take' new_client) in
      let try_chaos = Mtime.wait_for' next_chaos `Try_chaos in
      Action.choose [stop; newc; try_chaos]
    in
    match Action.invoke next_step with
    | `Stop r -> r
    | `New client ->
        let clients = List.filter client_is_live (client :: clients) in
        loop serve new_client clients last_chaos
    | `Try_chaos ->
        let clients = List.filter client_is_live clients in
        log "Try chaos (%d clients)" (List.length clients);
        try_chaos serve clients;
        loop serve new_client clients (Mtime.counter ())
  in
  loop serve new_client [] (Mtime.counter ())

let pong_server ~endpoint =
  Fun.Async.call @@ fun () ->
  Result.join @@ Net.with_listener ~on:endpoint @@ fun l ->
  log "Waiting for pings on %a" Net.Listener.pp l;
  let new_client = Cell.Drop.make () in
  let serve = serve l new_client in
  chaos serve new_client

(* Ping pong *)

let ping_or_pong ~endpoint ~max_ping_period_ms =
  log "Contacting pong server on %a" Net.Endpoint.pp endpoint;
  let* did_ping = Fun.Async.get (ping ~peer:endpoint ~max_ping_period_ms) in
  if did_ping then Ok () else begin
    log "Could not connect, will pong instead.";
    Fun.Async.get (pong_server ~endpoint)
  end

let main () =
  Unix.main @@ fun () ->
  let relax () = Mtime.wait_for (random_span_ms ~min:300 ~max:600 ()) in
  let endpoint = `Host ("localhost", 10101) in
  let max_ping_period_ms = 600 in
  let rec loop () = match ping_or_pong ~endpoint ~max_ping_period_ms with
  | Ok () -> relax (); loop ()
  | Error e -> log "%s: %s" error_str e; relax (); loop ()
  in
  loop ()

let () = if !Sys.interactive then () else exit (main ())
