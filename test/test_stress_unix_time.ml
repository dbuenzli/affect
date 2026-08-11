(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_std
open B0_testing
open Affect
open Affect_unix

let domain_count = Test_common.domain_count

let trace_mtime_wait ~exp ~actual =
  let pp = Mtime.Span.pp in
  Fun.Async.trace "Waited for %a +%a" pp exp pp (Mtime.Span.abs_sub actual exp)

let trace_ptime_wait ~exp ~actual =
  let diff = Ptime.Span.to_mtime_span (Ptime.diff actual exp) |> Option.get in
  Fun.Async.trace "Waited until %a +%a" Ptime.pp exp Mtime.Span.pp diff

let test_parallel_mtime  =
  Test.test' domain_count "stress parallel Mtime.wait_for" @@
  fun domain_count ->
  Unix.main ?domain_count @@ fun () ->
  let parallel_count = Fun.Async.parallel_count () in
  for i = 1 to 1000 * parallel_count do ignore @@ Fun.Async.call @@ fun () ->
    let dur = Mtime.Span.(1 * ms) in
    let actual = Mtime.observe_wait_for dur in
    trace_mtime_wait ~exp:dur ~actual
  done;
  ()

let test_parallel_seq_mtime  =
  Test.test' domain_count "stress parallel/seq Mtime.wait_for" @@
  fun domain_count ->
  Unix.main ?domain_count @@ fun () ->
  let parallel_count = Fun.Async.parallel_count () in
  for i = 1 to parallel_count do
    ignore @@ Fun.Async.call @@ fun () ->
    for i = 1 to 1000 do
      Fun.Async.get @@ Fun.Async.call @@ fun () ->
      let dur = Mtime.Span.(100 * us) in
      let actual = Mtime.observe_wait_for dur in
      trace_mtime_wait ~exp:dur ~actual
    done;
  done

let test_parallel_ptime  =
  Test.test' domain_count "stress parallel Ptime.wait_until" @@
  fun domain_count ->
  Unix.main ?domain_count @@ fun () ->
  let parallel_count = Fun.Async.parallel_count () in
  for i = 1 to 1000 * parallel_count do ignore @@ Fun.Async.call @@ fun () ->
    let dur = Ptime.Span.of_mtime_span Mtime.Span.(1 * ms) in
    let t = Ptime.add_span (Ptime.now ()) dur |> Option.get in
    let actual = Ptime.observe_wait_until t in
    trace_ptime_wait ~exp:t ~actual
  done;
  ()

let test_parallel_seq_ptime  =
  Test.test' domain_count "stress parallel/seq Ptime.wait_until" @@
  fun domain_count ->
  Unix.main ?domain_count @@ fun () ->
  let parallel_count = Fun.Async.parallel_count () in
  for i = 1 to parallel_count do
    ignore @@ Fun.Async.call @@ fun () ->
    for i = 1 to 1000 do
      Fun.Async.get @@ Fun.Async.call @@ fun () ->
      let dur = Ptime.Span.of_mtime_span Mtime.Span.(100 * us) in
      let t = Ptime.add_span (Ptime.now ()) dur |> Option.get in
      let actual = Ptime.observe_wait_until t in
      trace_ptime_wait ~exp:t ~actual
    done;
  done


let () = if !Sys.interactive then () else exit (Test_common.main ())
