(*---------------------------------------------------------------------------
   Copyright (c) 2024 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect;;

#install_printer Affect.Fun.Async.pp;;
#install_printer Affect.Fun.Async.Call.pp;;

let affect_toggle_tracing =
  let enabled = ref false in
  fun () ->
    enabled := not !enabled;
    let reporter =
      if !enabled then Affect.Fun.Async.Trace.stderr_reporter else
      Affect.Fun.Async.Trace.default_reporter
    in
    Affect.Fun.Async.Trace.set_reporter reporter
;;

let affect_top () =
  Affect.Fun.Async.main @@ fun () ->
  Toploop.loop Format.std_formatter
;;
