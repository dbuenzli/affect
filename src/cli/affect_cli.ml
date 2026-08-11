(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Cmdliner

let default_count_env = Cmd.Env.info "PARALLEL_COUNT"

let domain_count =
  let parser s = match Arg.Conv.parser Arg.int s with
  | Error _ as e ->  e
  | Ok c when c >= 1 -> Ok c
  | Ok c ->
      Error (Printf.sprintf "invalid value %d, must be greater than zero" c)
  in
  Arg.Conv.of_conv ~parser Arg.int

let parallel_count
    ?absent ?(opts = ["P"; "parallel-count"]) ?(env = default_count_env) ()
  =
  let doc = "$(docv) is the total number of parallel processors to use." in
  let docv = "COUNT" in
  let absent = match absent with
  | None -> Printf.sprintf "$(b,%d)" (Domain.recommended_domain_count ())
  | Some absent -> absent
  in
  Arg.(value & opt (some domain_count) None & info opts ~doc ~docv ~absent ~env)

let default_trace_env = Cmd.Env.info "PARALLEL_TRACE"

let parallel_trace
    ?(opts = ["T"; "parallel-trace"]) ?(env = default_trace_env) ()
  =
  let doc = "Enable tracing of parallel activities on $(b,stderr)." in
  Arg.(value & flag & info opts ~doc ~env)

let set_parallel_trace ?opts ?env ?reporter () =
  let default = Affect.Fun.Async.Trace.stderr_reporter in
  let reporter = Option.value ~default reporter in
  let enable_trace doit =
    if doit then Affect.Fun.Async.Trace.set_reporter reporter else ()
  in
  Term.map enable_trace (parallel_trace ?opts ?env ())
