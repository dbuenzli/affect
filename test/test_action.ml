(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Affect

(* The basics of action is tested in [test_affect.ml] this add a couple
   more tests. *)

let domain_count = Test_common.domain_count

let test_call_basics =
  Test.test' domain_count "Action.choose Fun.Async.get" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let f0 = Fun.Async.call @@ fun () -> Fun.Async.yield (); 0 in
  let f1 = Fun.Async.call @@ fun () -> Fun.Async.yield (); 1 in
  let f2 = Fun.Async.call @@ fun () -> Fun.Async.yield (); 2 in
  let choose = Action.choose Fun.Async.[get' f0; get' f1; get' f2] in
  let v = Action.invoke choose in
  (* B0_testing: would be nice to be able to choose between a set of values *)
  Test.holds (v = 0 || v = 1 || v = 2);
  ()

let test_unfair =
  Test.test' domain_count "Action.choose's unfairness" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let f = Fun.Async.call @@ fun () -> 0 in
  Fun.Async.yield ();
  let never_f = Action.choose [Action.always 1; Fun.Async.get' f] in
  let maybe_f = Action.choose [Fun.Async.get' f; Action.always 1] in
  Test.int (Action.invoke never_f) 1;
  let maybe_f = Action.invoke maybe_f in
  Test.holds (maybe_f = 1 || maybe_f = 0);
  ()

let test_never =
  Test.test' domain_count "Action.never" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let f = Fun.Async.call @@ fun () -> 0 in
  Fun.Async.yield ();
  let always_f_0 = Action.choose [Action.never; Fun.Async.get' f] in
  let always_f_1 = Action.choose [Fun.Async.get' f; Action.never] in
  Test.int (Action.invoke always_f_0) 0;
  Test.int (Action.invoke always_f_1) 0;
  ()

let test_map =
  Test.test' domain_count "Action.map" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let f0 = Fun.Async.call @@ fun () -> Fun.Async.yield (); 0 in
  let succ = Action.map succ (Fun.Async.get' f0) in
  let never = Action.map (fun _ -> assert false) Action.never in
  Test.int (Action.invoke @@ Action.choose [succ; never]) 1;
  ()

let test_guard =
  Test.test' domain_count "Action.guard" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let identify_sync_instances action =
    (* N.B. also increments on unsucessful [Action.poll]s *)
    let id = Atomic.make 0 in
    Action.guard @@ fun () ->
    let id = Atomic.fetch_and_add id 1 in
    Action.map (fun v -> id, v) action
  in
  let f0 = Fun.Async.call @@ fun () -> Fun.Async.yield (); 7 in
  let id_syncs = identify_sync_instances (Fun.Async.get' f0) in
  let test = Test.(pair T.int T.int) in
  test (Action.invoke id_syncs) (0, 7) ~__POS__;
  test (Action.invoke id_syncs) (1, 7) ~__POS__;
  let[@alert "-deprecated"] () =
    Test.(option (T.pair T.int T.int)) (Action.invoke_poll id_syncs)
      (Some (2, 7))
  in
  ()

let test_typing =
  Test.test' domain_count "Action typing" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let f0 = Fun.Async.call @@ fun () -> `Left in
  let f1 = Fun.Async.call @@ fun () -> `Right in
  let chosen = Action.invoke (Action.choose Fun.Async.[get' f0; get' f1]) in
  Test.holds (chosen = `Left || chosen = `Right);
  ()

let () = if !Sys.interactive then () else exit (Test_common.main ())
