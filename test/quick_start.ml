(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Affect

let main () =
  Fun.Async.main @@ fun () ->
  let f0 = Fun.Async.call (fun () -> 1) in
  let f1 = Fun.Async.call (fun () -> 2) in
  print_int (Fun.Async.get f0 + Fun.Async.get f1);
  0

let () = if !Sys.interactive then () else exit (main ())
