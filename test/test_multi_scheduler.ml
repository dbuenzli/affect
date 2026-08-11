(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Affect

let domain_count = Test_common.domain_count

let divide domain_count =
  let domain_count = match domain_count with
  | None -> Domain.recommended_domain_count () | Some count -> count
  in
  Int.max (domain_count / 2) 1

let test_don't_miss_wakeup =
  Test.test "Don't miss domain wakeup" @@ fun () ->
  (* Shows up easily when each scheduler has a single domain *)
  let domain_count = 1 in
  let port = Port.make () in
  let dom = Domain.spawn @@ fun () ->
    Fun.Async.main ~domain_count @@ fun () ->
    Fun.Async.call_trap_exn @@ fun () ->
    Test.string (Port.take port) "ping";
  in
  begin Fun.Async.main ~domain_count @@ fun () ->
    Fun.Async.call_trap_exn @@ fun () ->
    Port.offer port "ping";
  end;
  Domain.join dom;
  ()

let test_call_basics =
  Test.test' domain_count "Ping-pong between two Fun.Async.main" @@
  fun domain_count ->
  let domain_count = divide domain_count in
  let port = Port.make () in
  let cancel_me = Port.make () in
  let dom = Domain.spawn @@ fun () ->
    Fun.Async.main ~domain_count @@ fun () ->
    let f = Fun.Async.call @@ fun () ->
      Test.string (Port.take port) "ping";
      Port.offer port "pong";
    in
    Fun.Async.get f;
    let blocked = Fun.Async.call @@ fun () ->
      try ignore (Port.take port) with Fun.Async.Cancelled -> ()
    in
    Port.offer cancel_me blocked
  in
  begin Fun.Async.main ~domain_count @@ fun () ->
    let f = Fun.Async.call @@ fun () ->
      Port.offer port "ping";
      Test.string (Port.take port) "pong";
    in
    Fun.Async.get f;
    Fun.Async.cancel (Port.take cancel_me);
  end;
  Domain.join dom;
  ()

let () = if !Sys.interactive then () else exit (Test_common.main ())
