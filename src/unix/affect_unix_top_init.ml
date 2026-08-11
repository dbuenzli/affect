(*---------------------------------------------------------------------------
   Copyright (c) 2024 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect_unix;;

#install_printer Affect_unix.Mtime.Span.pp_ns;;
#install_printer Affect_unix.Mtime.pp;;
#install_printer Affect_unix.Ptime.Span.pp_d_ps;;
#install_printer Affect_unix.Ptime.pp;;
#install_printer Affect_unix.Unix.Signal.pp;;

let affect_unix_top () =
  Affect_unix.Unix.main @@ fun () -> Toploop.loop Format.std_formatter;;
