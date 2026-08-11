(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect

let tool ~domain_count =
  Fun.Async.main ?domain_count @@ fun () ->
  0

open Cmdliner
open Cmdliner.Term.Syntax

let tool_cmd =
  Cmd.make (Cmd.info "TODO" ~version:"%%VERSION%%") @@
  let+ domain_count = Affect_cli.parallel_count ()
  and+ () = Affect_cli.set_parallel_trace () in
  tool ~domain_count

let main () = Cmd.eval' tool_cmd
let () = if !Sys.interactive then () else exit (main ())
