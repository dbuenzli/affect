(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(* N.B. this is mostly a cut and paste from Ptime, there's more
   comments there if you find something puzzling. *)

(* Timestamps *)

type t =
  (* POSIX day count from the epoch and picosecond POSIX time point in that
     day starting from 00:00:00 *)
  int * int64

let now = Affect_unix__timeline.ptime_now_d_ps

let jd_posix_epoch = 2_440_588          (* the Julian day of the POSIX epoch *)
let jd_ptime_min_stamp = 1_721_060      (* the Julian day of Ptime.min_stamp *)
let jd_ptime_max_stamp = 5_373_484      (* the Julian day of Ptime.max_stamp *)
let jd_to_date jd =
  (* From http://www.tondering.dk/claus/cal/julperiod.php#formula *)
  let a = jd + 32044 in
  let b = (4 * a + 3) / 146097 in
  let c = a - ((146097 * b) / 4) in
  let d = (4 * c + 3) / 1461 in
  let e = c - ((1461 * d) / 4) in
  let m = (5 * e + 2) / 153 in
  let day = e - ((153 * m + 2) / 5) + 1 in
  let month = m + 3 - (12 * (m / 10)) in
  let year = 100 * b + d - 4800 + (m / 10) in
  (year, month, day)

let ps_count_in_ps    =                      1L
let ps_count_in_ns    =                  1_000L
let ps_count_in_100ns =                100_000L
let ps_count_in_us    =              1_000_000L
let ps_count_in_100us =            100_000_000L
let ps_count_in_ms    =          1_000_000_000L
let ps_count_in_100ms =        100_000_000_000L
let ps_count_in_s     =      1_000_000_000_000L
let ps_count_in_min   =     60_000_000_000_000L
let ps_count_in_hour  =   3600_000_000_000_000L
let ps_count_in_day   = 86_400_000_000_000_000L
let ns_count_in_day   = 86_400_000_000_000L
let ps_day_max        = 86_399_999_999_999_999L

let day_min = jd_ptime_min_stamp - jd_posix_epoch
let day_max = jd_ptime_max_stamp - jd_posix_epoch

let epoch = (0, 0L)                   (* 1970-01-01 00:00:00 UTC *)
let min_stamp = (day_min, 0L)         (* 0000-01-01 00:00:00 UTC *)
let max_stamp = (day_max, ps_day_max) (* 9999-12-31 23:59:59 UTC *)

(* Time spans *)

module Span = struct

  (* Arithmetic *)

  let neg = Affect_unix__timeline.ptime_neg
  let add = Affect_unix__timeline.ptime_add
  let sub = Affect_unix__timeline.ptime_sub
  let abs (d, _ as s) = if d < 0 then neg s else s

  (* POSIX time spans *)

  type nonrec t = t

  let zero = (0, 0L)
  let v (d, ps as s) =
    if ps < 0L || ps > ps_day_max
    then invalid_arg (Format.sprintf "illegal ptime time span: (%d,%Ld)" d ps)
    else s

  let of_d_ps (d, ps as s) = if ps < 0L || ps > ps_day_max then None else Some s
  let unsafe_of_d_ps s = s
  let unsafe_of_d_ps_option s = s
  let to_d_ps s = s

  let of_int_s secs =
    let d = Stdlib.abs secs in
    let s = (d / 86_400, Int64.(mul (of_int (d mod 86_400)) ps_count_in_s)) in
    if secs < 0 then neg s else s

  let day_int_min = min_int / 86_400
  let day_int_max = max_int / 86_400
  let to_int_s (d, ps) =
    if d < day_int_min || d > day_int_max then None else
    let days_s = d * 86_400 in
    let day_s = Int64.(to_int (div ps ps_count_in_s)) (* always positive *) in
    let secs = days_s + day_s in
    if secs < days_s (* positive overflow *) then None else Some secs

  let min_int_float = float min_int
  let max_int_float = float max_int
  let of_float_s secs =
    if secs <> secs (* nan *) then None else
    let days = floor (secs /. 86_400.) in
    if days < min_int_float || days > max_int_float then None else
    let rem_s = mod_float secs 86_400. in
    let rem_s = if rem_s < 0. then 86_400. +. rem_s else rem_s in
    if rem_s >= 86_400. then
      (* Guard against a potential overflow in the computation of [rem_s] *)
      let days = days +. 1. in
      if days > max_int_float then None else
      Some (int_of_float days, 0L)
    else
    let frac_s, rem_s = modf rem_s in
    let rem_ps = Int64.(mul (of_float rem_s) ps_count_in_s) in
    let frac_ps = Int64.(of_float (frac_s *. 1e12)) in
    Some (int_of_float days, (Int64.add rem_ps frac_ps))

  let to_float_s (d, ps) =
    let days_s = (float d) *. 86_400. in
    let day_s = Int64.(to_float (div ps ps_count_in_s)) in
    let day_rem_ps = Int64.(to_float (rem ps ps_count_in_s)) in
    days_s +. day_s +. (day_rem_ps *. 1e-12)

  let of_mtime_span span =
    let span_ns = Affect_unix__mtime.Span.to_uint64_ns span in
    let d = Int64.(to_int (unsigned_div span_ns ns_count_in_day)) in
    let rem_ns = Int64.unsigned_rem span_ns ns_count_in_day in
    let ps = Int64.mul rem_ns 1000L in
    d, ps

  let to_mtime_span span =
    let max_d =
      (* maximal value that doesn't overflow namely:
         Int64.(unsigned_div (sub (-1L) (sub ns_count_in_day 1L)
                ns_count_in_day)) *)
      213502
    in
    let d, ps = span in
    if d > max_d then None else
    let ns = Int64.unsigned_div ps 1000L in
    let span = Int64.(add (mul (of_int d) ns_count_in_day) ns) in
    Some (Affect_unix__mtime.Span.of_uint64_ns span)

  (* Predicates *)

  let equal (d0, ps0) (d1, ps1) = Int.equal d0 d1 && Int64.equal ps0 ps1
  let compare (d0, ps0) (d1, ps1) =
    let c = Int.compare d0 d1 in
    if c <> 0 then c else Int64.compare ps0 ps1

  let is_shorter s ~than = compare s than < 0
  let is_longer s ~than = compare s than > 0

  (* Formatting *)

  let pp_d_ps ppf (d, ps) =
    if d = 0
    then Format.fprintf ppf "%Lups" ps
    else Format.fprintf ppf "%dd %Lups" d ps
end

(* Predicates *)

let equal = Span.equal
let compare = Span.compare
let is_earlier t ~than = compare t than = -1
let is_later t ~than = compare t than = 1

(* Converting *)

let of_span (d, _ as span) =
  if d < day_min || d > day_max then None else Some span

let to_span = Fun.id

let of_float_s secs = match Span.of_float_s secs with
| None -> None
| Some d -> of_span d

let to_float_s = Span.to_float_s

(* Arithmetic *)

let add_span t d = of_span (Span.add t d)
let sub_span t d = of_span (Span.sub t d)
let diff t1 t0 = Span.sub t1 t0

(* Formatting *)

let frac = 4 (* 4 fractional decimals *)
let frac_div = 100000000L
let s_frac_of_ps ps = Int64.(div (rem ps ps_count_in_s) frac_div)
let pp ppf (d, ps) =
  let jd = d + jd_posix_epoch in
  let y, m, d = jd_to_date jd in
  let hh = Int64.(to_int (div ps ps_count_in_hour)) in
  let hh_rem = Int64.rem ps ps_count_in_hour in
  let mm = Int64.(to_int (div hh_rem ps_count_in_min)) in
  let mm_rem = Int64.rem hh_rem ps_count_in_min in
  let ss = Int64.(to_int (div mm_rem ps_count_in_s)) in
  Format.fprintf ppf "%04d-%02d-%02d %02d:%02d:%02d.%0*Ld-00:00"
    y m d hh mm ss frac (s_frac_of_ps ps);
  ()

(* Waiting *)

open Affect
open Affect.Action.Private

let wait_until_poll t tag ~continue =
  if is_earlier (now ()) ~than:t
  then None
  else Some (continue (Action.Result.Value tag))

let wait_until_block t tag ~blocked =
  let now = now () in
  if is_earlier now ~than:t then
    let timeline = Affect_unix__timeline.get_domain_local () in
    let blocked = Action.Blocked.Value.make tag blocked in
    Affect_unix__timeline.add_ptime_deadline timeline ~now t blocked
  else
  Action.Blocked.synced_unblock ~candidate:(Action.Result.Value tag) blocked

let wait_until_key = Action.Meta.Key.make ~pp_value:pp ()
let wait_until_meta t =
  let bindings = Action.Meta.[Binding (wait_until_key, t)] in
  Action.Meta.make ~bindings ~name:"Ptime.wait_until" ()

let wait_until' t tag =
  let poll = wait_until_poll t tag in
  let block = wait_until_block t tag in
  Action.Primitive.make ~meta:(wait_until_meta t) ~poll ~block

let wait_until t = Action.invoke (wait_until' t ())
let observe_wait_until t = wait_until t; now ()
