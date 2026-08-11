(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Affect

let domain_count = Test_common.domain_count

let test_call =
  Test.test' domain_count "Fun.Async.call" @@ fun domain_count ->
  Fun.Async.main ?domain_count @@ fun () ->
  let parallel_count = Fun.Async.parallel_count () in
  for i = 1 to parallel_count * 10000 do
    ignore @@ Fun.Async.call @@ fun () ->
    Fun.Async.yield ();
    Fun.Async.trace "%d" i;
  done;
  ()

let () = if !Sys.interactive then () else exit (Test_common.main ())
