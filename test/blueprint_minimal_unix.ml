(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Affect
open Affect_unix
let main () = Unix.main @@ fun () -> 0
let () = if !Sys.interactive then () else exit (main ())
