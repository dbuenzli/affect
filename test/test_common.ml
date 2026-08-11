(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open B0_testing
open Affect
open Cmdliner
open Cmdliner.Term.Syntax

let fail_and_dump_trace_after ~timeout_s d f =
  let on_break _ =
    Fun.Async.Trace.Delayed.prerr d;
    Test.failstop "Blocked for %ds! Stopping test ungracefully…" timeout_s
  in
  let break = Sys.Signal_handle on_break in
  let old = Sys.signal Sys.sigalrm break in
  let finally () = ignore (Unix.alarm 0); ignore (Sys.signal Sys.sigalrm old) in
  try ignore (Unix.alarm timeout_s); Fun.protect ~finally f with
  | Sys.Break -> on_break ()

let () =
  (* Once b0 depends on >= 5 we should simply do that by default. *)
  Test.Log.set_out Format.get_std_formatter

(* Common test driver. Add argument for number of domains, tracing,
   repeating the tests and makes test logging synchronization safe. *)

let repeat =
  let doc = "Repeat the test $(docv) times." in
  let env = Cmd.Env.info "REPEAT" in
  Arg.(value & opt int 1 & info ["R"; "repeat"] ~env ~doc ~docv:"COUNT")

let trace_filter =
  let only_fun =
    let doc = "Only trace asynchronous function activity." in
    Some `Only_fun, Arg.info ["F"; "only-fun"] ~doc
  in
  let only_user =
    let doc = "Only trace asynchronous function user activity." in
    Some `Only_user, Arg.info ["U"; "only-user"] ~doc
  in
  Arg.(value & vflag None [only_fun; only_user])

let only_fun =
  let doc = "Only trace asynchronous function activity." in
  Arg.(value & flag & info ["F"; "only-fun"] ~doc ~docv:"COUNT")

let domain_count = Test.Arg.make ()
let reporter tr = Test.Log.msg "%a" Fun.Async.Trace.pp tr
let main () =
  Test.main' @@
  let+ count = Affect_cli.parallel_count ()
  and+ trace = Affect_cli.parallel_trace ()
  and+ trace_filter and+ repeat in
  let () =
    let r = Fun.Async.Trace.stderr_reporter in
    let r = match trace_filter with
    | None -> r
    | Some `Only_fun -> Fun.Async.Trace.only_fun r
    | Some `Only_user -> Fun.Async.Trace.only_user r
    in
    if trace then Fun.Async.Trace.set_reporter r
  in
  fun () ->
    for i = 1 to repeat do
      if repeat > 1 then Test.Log.msg "Run %a/%d" Test.Fmt.count i repeat;
      Test.autorun ~args:Test.Arg.[value domain_count count] ()
    done
