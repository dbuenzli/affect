(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

val main :
  ?unblocker:Affect__action.unblocker ->
  ?domain_spawn:((unit -> unit) -> unit Domain.t) ->
  ?domain_count:int ->
  ?schedule:Affect__async_fun.Schedule.t ->
  ?handler:Affect__async_fun.Call_handler.t -> (unit -> 'a) -> 'a

val trace : ('a, Format.formatter, unit, unit) format4 -> 'a
