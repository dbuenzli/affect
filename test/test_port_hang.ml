(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Affect

let domain_count = Test_common.domain_count

(* It is easier to witness hangs if poll phases are stubbed with [None]. *)

let count = 10_000_000

let test_port_hang =
  Test.test' domain_count "Hang port" @@ fun domain_count ->
  let delayed = Fun.Async.Trace.Delayed.make () in
  Fun.Async.Trace.set_reporter (Fun.Async.Trace.Delayed.reporter delayed);
  Fun.Async.main ?domain_count @@ fun () ->
  for i = 0 to 10_000_000 do
    Fun.Async.Trace.Delayed.clear delayed;
    Test_common.fail_and_dump_trace_after ~timeout_s:1 delayed @@ fun () ->
    let p = Port.make () in
    let all =
      [ Fun.Async.call (fun () -> Port.offer p "hey!");
        Fun.Async.call (fun () -> ignore (Port.take p));
        Fun.Async.call (fun () -> ignore (Port.take p));
        Fun.Async.call (fun () -> Port.offer p "ho!"); ]
    in
    ignore (Fun.Async.get_all all);
    if i mod 3000 = 0 then (Test.Log.msg "Not hanged %d/%d" i count)
  done;
  ()

let () = if !Sys.interactive then () else exit (Test_common.main ())
