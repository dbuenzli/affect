(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Result.Syntax
open Affect
open Affect_unix

let () = Fun.Async.Trace.(set_reporter (only_fun stderr_reporter))

let download_url url =
  let b = Buffer.create 255 in
  let h = Curl.init () in
  Curl.set_url h url;
  Curl.set_writefunction h (fun s -> Buffer.add_string b s; String.length s);
  let ret = match Affect_curl.perform h with
  | Curl.CURLE_OK -> Ok (Buffer.contents b)
  | e -> Error (Format.sprintf "%s: %s" url (Curl.strerror e))
  in
  Curl.cleanup h; ret

let trace_page r = Fun.Async.trace "%s" (Result.retract r)

let test_downloads () =
  let url1 = "https://erratique.ch/robots.txt" in
  let url2 = "https://erratique.ch/" in
  let p1 = Fun.Async.call (fun () -> download_url url1) in
  let p2 = Fun.Async.call (fun () -> download_url url2) in
  match Fun.Async.get_either p1 p2 with
  | Either.Left p1 -> trace_page p1; trace_page (Fun.Async.get p2)
  | Either.Right p2 -> trace_page p2; trace_page (Fun.Async.get p1)

let main () =
  Unix.main @@ fun () ->
  Curl.global_init Curl.CURLINIT_GLOBALALL;
  Affect_curl.init ();
  test_downloads ();
  Affect_curl.deinit ();
  Curl.global_cleanup ();
  0

let () = if !Sys.interactive then () else exit (main ())
