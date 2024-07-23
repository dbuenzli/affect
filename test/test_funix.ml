(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let log = Test.log
let flip = let () = Random.self_init () in Random.bool
let now_s () = Unix.gettimeofday ()

let test_sleeps () =
  Test.test "simple sleep" @@ fun () ->
  let main () =
    let rec loop start max n =
      if n > max then log " Took %02fs" (now_s () -. start) else
      let dur_s = 0.25 in
      log " Sleeping for %.02fs" dur_s;
      Funix.sleep_s dur_s;
      loop start max (n + 1)
    in
    loop (now_s ()) 3 0
  in
  ignore (Fiber.run ~unblock:Funix.unblock main)

let test_either_sleep () =
  Test.test "either shortest sleep" @@ fun () ->
  let left_ret = ref false in
  let right_abort = ref false in
  let main () =
    let left = Fiber.spawn @@ fun () ->
      Funix.sleep_s 0.5;
      left_ret := true
    in
    let right = Fiber.spawn @@ fun () ->
      (try Funix.sleep_s 1.0 with
      | Fiber.Abort -> right_abort := true; raise Fiber.Abort);
      assert false
    in
    match Fiber.either left right with
    | Some Either.Left () -> ()
    | Some Either.Right () -> assert false
    | None -> assert false
  in
  ignore (Fiber.run ~unblock:Funix.unblock main);
  assert (!left_ret);
  assert (!right_abort)

let main () =
  Test.main @@ fun () ->
  test_sleeps ();
  test_either_sleep ();
  ()

let () = if !Sys.interactive then () else exit (main ())
