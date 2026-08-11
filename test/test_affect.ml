(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Affect

let domain_count = Test_common.domain_count

let test_call_basics =
  Test.test' domain_count "Fun.Async.call" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let _r0 = Fun.Async.call @@ fun () -> Fun.Async.yield () in
  let _r1 = Fun.Async.call @@ fun () -> Fun.Async.yield () in
  Fun.Async.yield ();
  Fun.Async.yield ();
  ()

let test_call_structured =
  Test.test' domain_count "Fun.Async.call is structured" @@ fun domain_count ->
  let f0_end = Atomic.make false in
  let f1_end = Atomic.make false in
  let f10_end = Atomic.make false in
  let f11_end = Atomic.make false in
  let () = Fun.Async.main ?domain_count @@ fun () ->
    let _f0 = Fun.Async.call @@ fun () -> Atomic.set f0_end true in
    let _f1 = Fun.Async.call @@ fun () ->
      let _f10 = Fun.Async.yield (); Atomic.set f10_end true in
      let _f11 = Fun.Async.yield (); Atomic.set f11_end true in
      Atomic.set f1_end true
    in
    if Atomic.get f1_end then begin
      Test.holds (Atomic.get f10_end) ~__POS__;
      Test.holds (Atomic.get f11_end) ~__POS__;
    end
  in
  Test.holds (Atomic.get f0_end) ~__POS__;
  Test.holds (Atomic.get f1_end) ~__POS__;
  Test.holds (Atomic.get f10_end) ~__POS__;
  Test.holds (Atomic.get f11_end) ~__POS__;
  ()

let test_call_handlers =
  Test.test' domain_count "Fun.Async.Call_handler" @@ fun domain_count ->
  let make_level_handler () =
    let c = Atomic.make 0 in
    c, Fun.Async.Call_handler.{ handle = fun f -> Atomic.incr c; f () }
  in
  let l0, l0_handler = make_level_handler () in
  let l1, l1_handler = make_level_handler () in
  let l2, l2_handler = make_level_handler () in
  begin Fun.Async.main ?domain_count ~handler:l0_handler @@ fun () ->
    Test.int (Atomic.get l0) 1;
    ignore @@ Fun.Async.call ~handler:l1_handler @@ fun () ->
    Test.int (Atomic.get l0) 2;
    Test.int (Atomic.get l1) 1;
    Fun.Async.call ~handler:l2_handler @@ fun () ->
    Test.int (Atomic.get l0) 3;
    Test.int (Atomic.get l1) 2;
    Test.int (Atomic.get l2) 1;
  end;
  Test.int (Atomic.get l0) 3;
  Test.int (Atomic.get l1) 2;
  Test.int (Atomic.get l2) 1;
  ()

(* This can be folded in the test with OCaml >= 5.5 *)
type _ Effect.t += Incr : int Effect.t

let test_structured_effect =
  Test.test' domain_count "Fun.Async.Call_handler structured effect handling" @@
  fun domain_count ->
  (* let type _ Effect.t += Incr : int Effect.t in OCaml > 5.5 *)
  let incr () = Effect.perform Incr in
  let make_counter () =
    let c = Atomic.make 0 in
    let handle f = match f () with
    | v -> v
    | effect Incr, k -> Atomic.incr c; Effect.Deep.continue k (Atomic.get c)
    in
    c, Fun.Async.Call_handler.{handle}
   in
   let c0, c0_incr = make_counter () in
   let c1, c1_incr = make_counter () in
   begin Fun.Async.main ?domain_count ~handler:c0_incr @@ fun () ->
     Test.int (incr ()) 1;
     Test.int (Atomic.get c0) 1;
     Test.int (Atomic.get c1) 0;
     ignore @@ Fun.Async.call @@ fun () ->
     Test.int (incr ()) 2;
     Test.int (Atomic.get c0) 2;
     Test.int (Atomic.get c1) 0;
     Fun.Async.call ~handler:c1_incr @@ fun () ->
     Test.int (incr ()) 1;
     Test.int (Atomic.get c0) 2;
     Test.int (Atomic.get c1) 1;
   end;
   Test.int (Atomic.get c0) 2;
   Test.int (Atomic.get c1) 1;
   ()

let test_get =
  Test.test' domain_count "Fun.Async.get" @@ fun domain_count ->
  Test.int begin
    Fun.Async.main ?domain_count @@ fun () ->
    let r0 = Fun.Async.call @@ fun () -> Fun.Async.yield (); 1 in
    let r1 = Fun.Async.call @@ fun () ->
      Fun.Async.yield (); Fun.Async.get r0 + 1
    in
    let r2 = Fun.Async.call @@ fun () -> raise Exit in
    Fun.Async.yield ();
    Test.int (Fun.Async.get r0) 1;
    Test.int (Fun.Async.get r1) 2;
    Test.raises Exit (fun () -> Fun.Async.get r2) ;
    Fun.Async.yield ();
    Fun.Async.get r1
  end 2;
  ()

let test_get_block =
  Test.test' domain_count "Fun.Async.get correct blocking" @@ fun dc ->
  (* An early implementation forgot acting on the action state when
     succeeding the synchronization during [block]. This would lead the
     function [unblock] being called twice and thus the
     continuation being potentially resumed twice. For that to happen the
     blocking phase of [one] needs to successfuly succeed synchronisation
     of [v0] and [v1] and both would unblock, it was still relatively
     easy to consistently witness before 1000 runs.  *)
  Fun.Async.main ?domain_count:dc @@ fun () ->
  let v0 = Fun.Async.call @@ fun () -> Fun.Async.yield () in
  let v1 = Fun.Async.call @@ fun () -> Fun.Async.yield () in
  let v2 = Fun.Async.call @@ fun () ->
    let one = Action.choose [Fun.Async.get' v0; Fun.Async.get' v1] in
    Action.invoke one;
  in
  Fun.Async.get v2;
  ()

let test_main_exn_ret =
  Test.test' domain_count "Fun.Async.main exn return" @@ fun domain_count ->
  Test.raises Exit begin fun () ->
    Fun.Async.main ?domain_count @@ fun () -> Fun.Async.yield (); raise Exit
  end;
  ()

let test_port_basics =
  Test.test' domain_count "Port" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let p = Port.make () in
  let v = Fun.Async.call (fun () -> Port.take p) in
  ignore @@ Fun.Async.call (fun () -> Port.offer p "hey!");
  Test.string  (Fun.Async.get v) "hey!";
  (* Waiting offers *)
  ignore @@ Fun.Async.call (fun () -> Port.offer p "ha"; Port.offer p "hu");
  Fun.Async.yield ();
  Test.string (Port.take p) "ha";
  Test.string (Port.take p) "hu";
  (* Waiting takers *)
  let v = Fun.Async.call @@ fun () ->
    let s1 = Port.take p in
    let s2 = Port.take p in
    s1 ^ s2
  in
  Port.offer p "hey";
  Port.offer p "ho";
  Test.string (Fun.Async.get v) "heyho";
  ()

let test_port_no_self_sync =
  Test.test' domain_count "Port self choice correctness" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let propose' p v =
    let ours = Port.offer' p v v in
    let theirs = Port.take' p in
    Action.choose [ours; theirs]
  in
  let propose p v = Action.invoke (propose' p v) in
  let p = Port.make () in
  let v0 = Fun.Async.call @@ fun () -> propose p "hey!" in
  let v1 = Fun.Async.call @@ fun () -> propose p "ho!" in
  let v0, v1 = Fun.Async.get v0, Fun.Async.get v1 in
  Test.string v0 v0;
  Test.holds (v0 = "hey!" || v0 = "ho!");
  (* Check that it really never self syncs in the same choice *)
  let unblock = Port.make () in
  let only_unblock = Action.choose [propose' p "never"; Port.take' unblock] in
  let v2 = Fun.Async.call @@ fun () -> Action.invoke only_unblock in
  Fun.Async.yield ();
  Port.offer unblock "unblock";
  Test.string (Fun.Async.get v2) "unblock";
  ()

let test_cell_drop =
  Test.test' domain_count "Cell.Drop" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let c = Cell.Drop.make () in
  Cell.Drop.put c 1;
  Test.bool (Cell.Drop.is_empty c) false;
  Test.int (Cell.Drop.take c) 1;
  Test.bool (Cell.Drop.is_empty c) true;
  ignore (Fun.Async.call @@ fun () -> Cell.Drop.put c 2; Cell.Drop.put c 3);
  Test.int (Cell.Drop.take c) 2;
  Test.int (Cell.Drop.take c) 3;
  let c = Cell.Drop.make () in
  let v = Fun.Async.call @@ fun () ->
    let v0 = Cell.Drop.take c in v0 ^ Cell.Drop.take c
  in
  Fun.Async.get @@ (Fun.Async.call @@ fun () -> Cell.Drop.put c "hey");
  Cell.Drop.put c "ho";
  Test.string (Fun.Async.get v) "heyho";
  Test.bool (Cell.Drop.is_empty c) true;
  ()

let test_cell_no_self_sync =
  Test.test' domain_count "Cell.Drop self choice correctness" @@ fun dc ->
  (* It's difficult to devise a test case that exhibit a block when
     the self test fails because the poll phase succeeds for one other
     in all cases (perhaps it mean that we could avoid the self test?). However
     it quickly shows if we stub the cell's actions polling phase with None. *)
  Fun.Async.main ?domain_count:dc @@ fun () ->
  let propose' c v =
    let ours = Action.map (fun () -> v ^ "(put)") (Cell.Drop.put' c v ()) in
    let theirs = Action.map (fun v -> v ^ "(take)") (Cell.Drop.take' c) in
    Action.choose [theirs; ours]
  in
  let propose c v = Action.invoke (propose' c v) in
  let c = Cell.Drop.make () in
  let v0 = Fun.Async.call @@ fun () -> propose c "hey!" in
  Test.string (Fun.Async.get v0) "hey!(put)";
  Test.bool (Cell.Drop.is_empty c) false ~__POS__;
  let v1 = Fun.Async.call @@ fun () -> propose c "ho!" in
  Test.string (Fun.Async.get v1) "hey!(take)";
  Test.bool (Cell.Drop.is_empty c) true ~__POS__;
  ()

let test_cell_lazy =
  Test.test' domain_count "Cell.Lazy" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let a = Atomic.make 0 in
  let c = Cell.Lazy.make (fun () -> Atomic.incr a; 2 * 3) in
  Test.bool (Cell.Lazy.is_forced c) false;
  let v0 = Fun.Async.call @@ fun () -> Cell.Lazy.force c in
  let v1 = Fun.Async.call @@ fun () -> Cell.Lazy.force c in
  Test.int (Cell.Lazy.force c) 6;
  Test.bool (Cell.Lazy.is_forced c) true;
  Test.int (Fun.Async.get v0) 6;
  Test.int (Fun.Async.get v1) 6;
  Test.int (Atomic.get a) 1;
  ()

let test_cell_once =
  Test.test' domain_count "Cell.Once" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let c = Cell.Once.make () in
  Test.bool (Cell.Once.is_set c) false;
  let v0 = Fun.Async.call @@ fun () -> Cell.Once.try_set_is_ours c 1 in
  let v1 = Fun.Async.call @@ fun () -> Cell.Once.try_set_is_ours c 2 in
  let v = Cell.Once.get c in
  Test.bool (Cell.Once.is_set c) true;
  Test.holds (v = 1 || v = 2);
  Test.holds (Bool.logxor (Fun.Async.get v0) (Fun.Async.get v1));
  Test.bool (Cell.Once.try_set_is_ours c 3) false;
  Test.holds (v = 1 || v = 2);
  ()

let test_semaphore =
  Test.test' domain_count "Semaphore" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  Snap.raise (fun () -> Semaphore.make (-2)) @> __POS_OF__
    (Invalid_argument "negative capacity -2");
  Snap.raise (fun () -> Semaphore.release (Semaphore.make 2)) @> __POS_OF__
    (Invalid_argument "overflowing the capacity of 2");
  let sem = Semaphore.make 2 in
  let count = Atomic.make 0 in
  let incr () =
    Fun.Async.yield ();
    Semaphore.with_acquired sem @@ fun () ->
    Atomic.incr count;
    Fun.Async.yield ();
    let c = Atomic.get count in
    Test.holds (c = 1 || c = 2);
    Atomic.decr count
  in
  let incrs = List.init 10(fun _ -> Fun.Async.call incr) in
  let _l = Fun.Async.get_all incrs in
  ()

let test_cancel_basics =
  Test.test' domain_count "Fun.Async cancellation basics" @@
  fun domain_count -> Fun.Async.main ?domain_count @@ fun () ->
  let ctrl_s0 = Port.make () in
  let ctrl_s1 = Port.make () in
  let f0 = Fun.Async.call @@ fun () ->
    Test.bool (Fun.Async.is_current_cancelled ()) false;
    let s0 = Fun.Async.call @@ fun () ->
      Test.bool (Fun.Async.is_current_cancelled ()) false;
      Port.offer ctrl_s0 ();
      (try Port.take ctrl_s0 with Fun.Async.Cancelled -> ());
      Test.bool (Fun.Async.is_current_cancelled ()) true;
      2
    in
    let s1 = Fun.Async.call @@ fun () ->
      Test.bool (Fun.Async.is_current_cancelled ()) false;
      Port.offer ctrl_s1 ();
      (try Port.take ctrl_s1 with Fun.Async.Cancelled -> ());
      Test.bool (Fun.Async.is_current_cancelled ()) true;
      Fun.Async.check_cancellation ()
    in
    Fun.Async.mask_cancellation @@ fun () ->
    Test.int (Fun.Async.get s0) 2;
    Test.raises Fun.Async.Cancelled (fun () -> Fun.Async.get s1);
    Test.bool (Fun.Async.is_cancelled s0) true;
    Test.bool (Fun.Async.is_cancelled s1) true;
    Test.bool (Fun.Async.is_current_cancelled ()) true;
    let s2 = Fun.Async.call @@ fun () ->
      Test.holds (Fun.Async.is_current_cancelled ());
      Fun.Async.check_cancellation ()
    in
    Test.raises Fun.Async.Cancelled (fun () -> Fun.Async.get s2);
    "hey"
  in
  (* Wait until s0 and s1 have checked a few assertions *)
  Port.take ctrl_s0;
  Port.take ctrl_s1;
  Fun.Async.cancel f0;
  Test.string (Fun.Async.get f0) "hey";
  Test.bool (Fun.Async.is_cancelled f0) true;
  ()

let test_wait_cancelled_action =
  Test.test' domain_count "Fun.Async.wait_cancelled" @@
  fun domain_count -> Fun.Async.main ?domain_count @@ fun () ->
  let f0 = Fun.Async.call @@ fun () ->
    (* Never say never *)
    try Action.invoke Action.never with Fun.Async.Cancelled -> ()
  in
  let _f1 = Fun.Async.call @@ fun () -> Fun.Async.cancel f0 in
  Action.invoke (Fun.Async.wait_cancelled f0 ());
  ()

let test_sys_break =
  Test.test' domain_count "Sys.Break exception behaviour" @@
  fun domain_count -> Fun.Async.main ?domain_count @@ fun () ->
  let call_handlers = Port.make () in
  let f0 = Fun.Async.call @@ fun () ->
    let f1 = Fun.Async.call @@ fun () -> Action.invoke Action.never in
    let f2 = Fun.Async.call @@ fun () -> Action.invoke Action.never in
    Port.offer call_handlers (f1, f2);
    raise Sys.Break;
  in
  let f1, f2 = Port.take call_handlers in
  Test.raises Fun.Async.Cancelled (fun () -> Fun.Async.get f1);
  Test.raises Fun.Async.Cancelled (fun () -> Fun.Async.get f2);
  Test.bool true (Fun.Async.is_cancelled f1);
  Test.bool true (Fun.Async.is_cancelled f2);
  Test.raises Fun.Async.Cancelled (fun () -> Fun.Async.get f0);
  ()

let test_parallel_count_override =
  Test.test' domain_count "Fun.Async.parallel_count_override" @@
  fun domain_count -> Fun.Async.main ?domain_count @@ fun () ->
  let domain_count = match domain_count with
  | None -> Domain.recommended_domain_count () | Some count -> count
  in
  Test.int (Fun.Async.parallel_count ()) domain_count;
  let count = domain_count + 1 in
  let override = Fun.Async.parallel_count_override count in
  ignore @@ Fun.Async.call ~handler:override @@ fun () ->
  Test.int (Fun.Async.parallel_count ()) count;
  ignore @@ Fun.Async.call @@ fun () ->
  Test.int (Fun.Async.parallel_count ()) count ~__POS__;
  ()

let test_divide_work =
  Test.test "Fun.Async.divide_work" @@ fun () ->
  let ranges ~size ~worker_count =
    let worker_count, range = Fun.Async.divide_work ~size ~worker_count in
    List.init worker_count range
  in
  let snap = Snap.(list T.(t2 int int)) in
  Snap.raise (fun () -> ranges ~size:0 ~worker_count:0) @> __POS_OF__
    (Invalid_argument "worker_count 0 not greater than 0");
  Snap.raise (fun () -> ranges ~size:(-1) ~worker_count:1) @> __POS_OF__
    (Invalid_argument("work size -1 is negative"));
  Snap.raise (fun () ->
      snd (Fun.Async.divide_work ~size:0 ~worker_count:3) 0) @> __POS_OF__
    (Invalid_argument "no ranges: work size was 0");
  Snap.raise (fun () ->
      snd (Fun.Async.divide_work ~size:2 ~worker_count:3) 2) @> __POS_OF__
    (Invalid_argument "worker index 2 not in range [0;1]");
  snap (ranges ~size:0 ~worker_count:1) @> __POS_OF__ [];
  snap (ranges ~size:1 ~worker_count:1) @> __POS_OF__ [(0, 0)];
  snap (ranges ~size:2 ~worker_count:1) @> __POS_OF__ [(0, 1)];
  snap (ranges ~size:0 ~worker_count:2) @> __POS_OF__ [];
  snap (ranges ~size:1 ~worker_count:2) @> __POS_OF__ [(0, 0)];
  snap (ranges ~size:2 ~worker_count:2) @> __POS_OF__ [(0, 0); (1, 1)];
  snap (ranges ~size:3 ~worker_count:2) @> __POS_OF__ [(0, 1); (2, 2)];
  snap (ranges ~size:4 ~worker_count:2) @> __POS_OF__ [(0, 1); (2, 3)];
  snap (ranges ~size:5 ~worker_count:2) @> __POS_OF__ [(0, 2); (3, 4)];
  snap (ranges ~size:1 ~worker_count:3) @> __POS_OF__ [(0, 0)];
  snap (ranges ~size:2 ~worker_count:3) @> __POS_OF__ [(0, 0); (1, 1)];
  snap (ranges ~size:3 ~worker_count:3) @> __POS_OF__ [(0, 0); (1, 1); (2, 2)];
  snap (ranges ~size:4 ~worker_count:3) @> __POS_OF__ [(0, 1); (2, 2); (3, 3)];
  snap (ranges ~size:5 ~worker_count:3) @> __POS_OF__ [(0, 1); (2, 3); (4, 4)];
  ()

let () = if !Sys.interactive then () else exit (Test_common.main ())
