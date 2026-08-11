(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

type uint64 = int64

module Span = struct
  (* N.B. this is a cut and paste from More.Mtime *)

  (* Time spans are by a nanosecond magnitude stored in an unsigned 64-bit
     integer. Allows to represent spans for ~584.5 Julian years. *)

  type t = uint64
  let zero = 0L
  let one = 1L
  let max_span = -1L

  let equal = Int64.equal
  let compare = Int64.unsigned_compare
  let is_shorter s ~than = compare s than < 0
  let is_longer s ~than = compare s than > 0

  let add = Int64.add
  let abs_sub s0 s1 =
    if compare s0 s1 < 0 then Int64.sub s1 s0 else Int64.sub s0 s1

  let sat_sub s0 s1 =
    if compare s0 s1 <= 0 then zero else Int64.sub s0 s1

  (* Durations *)

  let ( * ) n span = Int64.mul (Int64.of_int n) span
  let ( / ) span n = Int64.unsigned_div span (Int64.of_int n)
  let ( + ) = add
  let ( - ) = sat_sub
  let ns   =                      1L
  let us   =                  1_000L
  let ms   =              1_000_000L
  let s    =          1_000_000_000L
  let min  =         60_000_000_000L
  let hour =       3600_000_000_000L
  let day  =      86400_000_000_000L
  let year = 31_557_600_000_000_000L

  (* Conversions *)

  let to_uint64_ns s = s
  let of_uint64_ns ns = ns

  let max_float_int = 9007199254740992. (* 2^53. *)
  let int64_min_int_float = Int64.to_float Int64.min_int
  let int64_max_int_float = Int64.to_float Int64.max_int

  let of_float_ns sf =
    if sf < 0. || sf >= max_float_int || not (Float.is_finite sf)
    then None else Some (Int64.of_float sf)

  let to_float_ns s =
    if Int64.compare 0L s <= 0 then Int64.to_float s else
    int64_max_int_float +. (-. int64_min_int_float +. Int64.to_float s)

  let to_float_s s = 1e-9 *. to_float_ns s
  let of_float_s s = of_float_ns (s *. 1e9)

  let units = [|"ns"; "μs"; "ms"; "s"|]
  let pp ppf ns =
    let u = ref ns in
    let rem_u = ref 0L in
    let i = ref 0 in
    while (Int64.compare !u 1000L >= 0 && !i < 3) do
      rem_u := Int64.rem !u 1000L;
      u := Int64.(div !u 1000L);
      incr i;
    done;
    if !i = 0 then Format.fprintf ppf "%Luns" !u else
    let frac = Int64.div !rem_u 100L (* first decimal digit *) in
    Format.fprintf ppf "%Lu.%Lu%s" !u frac units.(!i)

  let pp_ns ppf s = Format.fprintf ppf "%Luns" s
end

(* Counters *)

type counter = Affect_unix__timeline.mtime_ns
let counter () = Affect_unix__timeline.mtime_now_ns ()
let count c = Int64.sub (Affect_unix__timeline.mtime_now_ns ()) c

let program_startup = counter ()
let elapsed () = count program_startup

(* Timestamps *)

type t = uint64
let now = Affect_unix__timeline.mtime_now_ns
let min_stamp = 0L
let max_stamp = -1L

(* Predicates *)

let equal = Int64.equal
let compare = Affect_unix__timeline.mtime_compare
let is_earlier t ~than = compare t than < 0
let is_later t ~than = compare t than > 0

(* Arithmetic *)

let span t0 t1 =
  if compare t0 t1 < 0 then Int64.sub t1 t0 else Int64.sub t0 t1

let add_span t span =
  let sum = Int64.add t span in
  if compare t sum <= 0 then Some sum else None

let sub_span t span =
  if compare t span < 0 then None else Some (Int64.sub t span)

(* Converting *)

let to_uint64_ns t = t
let of_uint64_ns ns = ns
let pp ppf s = Format.fprintf ppf "%Lu" s

(* Waiting *)

open Affect
open Affect.Action.Private

let wait_for_poll start span tag ~continue =
  if count start >= span
  then Some (continue (Action.Result.Value tag))
  else None

let wait_for_block start span tag ~blocked =
  if count start >= span then
    Action.Blocked.synced_unblock ~candidate:(Action.Result.Value tag) blocked
  else
  let timeline = Affect_unix__timeline.get_domain_local () in
  let deadline = Span.add start span in
  let blocked = Action.Blocked.Value.make tag blocked in
  Affect_unix__timeline.add_mtime_deadline timeline deadline blocked

let wait_for_key = Action.Meta.Key.make ~pp_value:Span.pp ()
let wait_for_meta span =
  let bindings = Action.Meta.[Binding (wait_for_key, span)] in
  Action.Meta.make ~name:"Mtime.wait_for" ~bindings ()

let wait_for' span tag =
  let start_wait span tag () =
    let start = counter () in
    let poll = wait_for_poll start span tag in
    let block = wait_for_block start span tag in
    Action.Primitive.make ~meta:(wait_for_meta span) ~poll ~block
  in
  Action.guard (start_wait span tag)

let wait_for span = Action.invoke (wait_for' span ())
let observe_wait_for span =
  let globally = counter () in
  Action.invoke (wait_for' span ());
  count globally
