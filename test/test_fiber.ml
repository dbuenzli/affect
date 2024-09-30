(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing

let log fmt = Test.log ("%a " ^^ fmt) Fiber.Handle.pp (Fiber.Handle.self ())
let unblock = Fiber.never_unblock

let fail_may_hang f = f () (* TODO timeout these *)

let rand = Test.Rand.state ()
let flip () = Random.State.bool rand

(* Functions with yield jitter and tracing *)

let may_yield () = if flip () then Fiber.yield ()
let async_val v = (* asyncs value [v] with yield jitter *)
  let f = Fiber.async @@ fun () -> may_yield (); v in
  may_yield (); f

let traced_may_yield () = if flip () then (log "Yielding"; Fiber.yield ())
let traced_async_val v =
  let f = Fiber.async @@ fun () -> traced_may_yield (); log "Done!"; v in
  log "Called %a" Fiber.pp f; traced_may_yield (); f

let traced_await pp f =
  log "Awaiting %a" (Fiber.pp' Test.Fmt.text_string) f;
  let v = Fiber.await f in
  log "Got %a" (Fiber.pp' pp) f; v

(* Tests *)

let test_basic_scheduling () =
  Test.test "basic scheduling" @@ fun () ->
  Fiber.main ~unblock @@ fun () ->
  log "I'm the main here";
  let msgs = ["Hey"; "ho,"; "let's go!"] in
  let fs = List.map traced_async_val msgs in
  let vs = List.map (traced_await Test.Fmt.text_string) fs in
  Test.(list ~elt:Eq.string) msgs vs ~__POS__;
  ()

let test_async_await () =
  Test.test "Fiber.{async,await}" @@ fun () ->
  Fiber.main ~unblock @@ fun () ->
  let f0 () = true in
  let f1 () = raise Exit in
  Test.bool (Fiber.await (Fiber.async f0)) true ~__POS__;
  Test.raises Exit (fun () -> Fiber.await (Fiber.async f1)) ~__POS__;
  ()

let test_fibers_are_structured () =
  Test.test "fibers are structured, wait for fibers" @@ fun () ->
  Fiber.main ~unblock @@ fun () ->
  let f0_returned = ref false in
  let f1_returned = ref false in
  let f0 () = Fiber.yield (); f0_returned := true in
  let f1 () = Fiber.yield (); f1_returned := true; raise Exit in
  let f2 () =
    let a, b = if flip () then f0, f1 else f1, f0 in
    ignore (Fiber.async a); ignore (Fiber.async b);
  in
  Fiber.await (Fiber.async f2);
  Test.holds !f0_returned ~__POS__;
  Test.holds !f1_returned ~__POS__;
  ()

let test_fiber_cancellation_is_structured () =
  Test.test "fibers cancellation is structured" @@
  fail_may_hang @@ fun () ->
  fun () ->
  Fiber.main ~unblock @@ fun () ->
  let f0 () = while not (Fiber.self_is_cancelled ()) do Fiber.yield () done in
  let f1 () = Fiber.await (Fiber.async f0) in
  let f2 () =
    let a0 = Fiber.async f1 in
    Fiber.self_cancel ();
    let a1 = Fiber.async f1 (* New fibers should be cancelled *) in
    ignore (Fiber.await a0);
    ignore (Fiber.await a1);
  in
  Fiber.await (Fiber.async f2);
  ()

let test_main_is_structured () =
  Test.test "main is structured, waits for fibers" @@ fun () ->
  let f0_returned = ref false in
  let f1_returned = ref false in
  begin Fiber.main ~unblock @@ fun () ->
    let f0 () = Fiber.yield (); f0_returned := true in
    let f1 () = Fiber.yield (); f1_returned := true; raise Exit in
    let a, b = if flip () then f0, f1 else f1, f0 in
    ignore (Fiber.async a); ignore (Fiber.async b);
  end;
  Test.holds !f0_returned ~__POS__;
  Test.holds !f1_returned ~__POS__;
  ()

let test_main_cancellation_is_structured () =
  Test.test "main cancellation is structured" @@ fun () ->
  fail_may_hang @@ fun () ->
  Fiber.main ~unblock @@ fun () ->
  let f0 () = while not (Fiber.self_is_cancelled ()) do Fiber.yield () done in
  let f1 () = Fiber.await (Fiber.async f0) in
  let a0 = Fiber.async f1 in
  Fiber.self_cancel ();
  let a1 = Fiber.async f1 (* New fibers should be cancelled *) in
  ignore (Fiber.await a0);
  ignore (Fiber.await a1);
  ()

let test_main_no_async_calls () =
  Test.test "main makes no async calls" @@ fun () ->
  Test.holds (Fiber.main ~unblock @@ fun () -> true) ~__POS__;
  Test.holds (Fiber.main ~unblock @@ fun () -> Fiber.yield (); true) ~__POS__;
  ()

let test_await_all () =
  Test.test "Fiber.await_all" @@ fun () ->
  Fiber.main ~unblock @@ fun () ->
  let async v =
    let af = Fiber.async @@ fun () -> if flip () then Fiber.yield (); v in
    if flip () then Fiber.yield (); af
  in
  let is = [1; 2; 3; 4] in
  let fs = List.map async is in
  let vs = Fiber.await_all fs in
  Test.(list ~elt:Eq.int) is vs ~__POS__;
  ()

let test_await_first () =
  Test.test "Fiber.await_first" @@ fun () ->
  Fiber.main ~unblock @@ fun () ->
  let is = [1; 2; 3; 4] in
  let fs = List.map async_val is in
  let rec collect acc = function
  | [] -> List.sort Int.compare acc
  | fs ->
      let v, fs = Fiber.await_first fs in
      collect (v :: acc) fs
  in
  let vs = collect [] fs in
  Test.(list ~elt:Eq.int) is vs ~__POS__;
  ()

let test_pick_either () =
  Test.test "Fiber.pick_either" @@ fun () ->
  fail_may_hang @@ fun () ->
  Fiber.main ~unblock @@ fun () ->
  begin
    let f () = for i = 0 to 4 do Fiber.yield () done in
    let g () = while not (Fiber.self_is_cancelled ()) do Fiber.yield () done in
    let f = Fiber.async f in
    let g = Fiber.async g in
    let a, b = if flip () then f, g else g, f in
    match Fiber.pick_either a b with
    | Either.Left () -> Test.holds (Fiber.cancelled b) ~__POS__
    | Either.Right () -> Test.holds (Fiber.cancelled a) ~__POS__
  end;
  begin
    (* Ignore Fiber.Cancelled *)
    let f () = raise Fiber.Cancelled in
    let g () = for i = 0 to 4 do Fiber.yield () done; true in
    let f = Fiber.async f in
    let g = Fiber.async g in
    let a, b = if flip () then f, g else g, f in
    match Fiber.pick_either a b with
    | Either.Left v -> Test.holds v ~__POS__
    | Either.Right v -> Test.holds v ~__POS__
  end;
  ()

let test_block () =
  Test.test "block" @@ fun () ->
  let unblock, block =
    let q = Queue.create () in
    let m = Mutex.create () in
    let unblock ~poll = Mutex.protect m @@ fun () -> Queue.take_opt q in
    let add_block f = Mutex.protect m @@ fun () -> Queue.add f q in
    unblock, add_block
  in
  let my_yield () =
    let block = block and cancel _ = false and return _ = () in
    Fiber.block ~block ~cancel ~return
  in
  Fiber.main ~unblock @@ fun () ->
  let f () = my_yield () in
  let f0 = Fiber.async f in
  let f1 = Fiber.async f in
  my_yield ();
  Fiber.await f0;
  Fiber.await f1;
  ()

let main () =
  Test.main @@ fun () ->
  test_basic_scheduling ();
  test_async_await ();
  test_fibers_are_structured ();
  test_fiber_cancellation_is_structured ();
  test_main_is_structured ();
  test_main_cancellation_is_structured ();
  test_main_no_async_calls ();
  test_await_all ();
  test_await_first ();
  test_pick_either ();
  test_block ();
  ()

let () = if !Sys.interactive then () else exit (main ())
