(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Affect
let main () = Fun.Async.main @@ fun () -> 0
let () = if !Sys.interactive then () else exit (main ())
