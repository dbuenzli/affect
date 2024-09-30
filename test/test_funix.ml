(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let log fmt = Test.log ("%a " ^^ fmt) Fiber.Handle.pp (Fiber.Handle.self ())
let unblock = Funix.unblock

let rand = Test.Rand.state ()
let flip () = Random.State.bool rand
let now_s () = Unix.gettimeofday ()
let test_sleeps () =
  Test.test "simple sleep" @@ fun () ->
  Fiber.main ~unblock @@ fun () ->
  let rec f start n =
    if n = 0 then () else
    let dur_s = 0.25 in
    log " Sleeping for %.02fs" dur_s;
    Funix.sleep_s dur_s;
    f start (n - 1)
  in
  let start = now_s () in
  f start 4;
  log " Took %02fs" (now_s () -. start)

let test_either_sleep () =
  Test.test "Fiber.pick_either shortest sleep" @@ fun () ->
  Fiber.main ~unblock @@ fun () ->
  let left_returns = ref false in
  let right_cancel = ref false in
  let left () = Funix.sleep_s 0.5; left_returns := true in
  let right () = try Funix.sleep_s 1.0; Test.fail "Unexpected" ~__POS__ with
  | Fiber.Cancelled -> right_cancel := true
  in
  let left = Fiber.async left in
  let right = Fiber.async right in
  begin match Fiber.pick_either left right with
  | Either.Left () -> ()
  | Either.Right () -> Test.fail "Unexpected" ~__POS__
  end;
  Test.holds !left_returns ~__POS__;
  Fiber.await right;
  Test.holds !right_cancel ~__POS__;
  ()

let main () =
  Test.main @@ fun () ->
  test_sleeps ();
  test_either_sleep ();
  ()

let () = if !Sys.interactive then () else exit (main ())
