(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect__base
open Affect.Action.Private

(* Monotonic clock

   We have the minimal set of operations for the timeline here, the rest
   is in [Affect_unix__mtime.ml]. *)

type uint64 = int64
type mtime_ns = uint64
type mtime_span_ns = uint64
external mtime_now_ns : unit -> mtime_ns = "ocaml_affect_mtime_now_ns"

let int64_min_int_float = Int64.to_float Int64.min_int
let int64_max_int_float = Int64.to_float Int64.max_int
let mtime_span_ns_to_float_ns span =
  if Int64.compare 0L span <= 0 then Int64.to_float span else
  int64_max_int_float +. (-. int64_min_int_float +. Int64.to_float span)

let mtime_span_ns_to_float_s span = 1e-9 *. mtime_span_ns_to_float_ns span
let mtime_span_ns_to_int_ms span =
  match Int64.unsigned_to_int (Int64.unsigned_div span 1_000_000L) with
  | None -> max_int
  | Some ms -> ms

let mtime_compare = Int64.unsigned_compare
let mtime_max_stamp = -1L
let mtime_add_span t span =
  let sum = Int64.add t span in
  if mtime_compare t sum <= 0 then Some sum else None

(* POSIX clock

   We have the minimal set of operations for the timeline here, the rest
   is in [Affect_unix__posix.ml]. *)

type ptime_d_ps = int * int64
external ptime_now_d_ps : unit -> ptime_d_ps = "ocaml_affect_ptime_now_d_ps"

let ps_count_in_day = 86_400_000_000_000_000L
let ns_count_in_day = 86_400_000_000_000L

let ptime_neg = function
| (d, 0L)  -> (-d, 0L)
| (d, ps) -> (-(d + 1), Int64.sub ps_count_in_day ps)

let ptime_add (d0, ps0) (d1, ps1) =
  let d = d0 + d1 in
  let ps = Int64.add ps0 ps1 in
  let ps_clamp = Int64.rem ps ps_count_in_day in
  let d = d + Int64.compare ps ps_clamp in
  d, ps_clamp

let ptime_sub s0 s1 = ptime_add s0 (ptime_neg s1)

let ptime_compare (d0, ps0) (d1, ps1) =
  let c = Int.compare d0 d1 in
  if c <> 0 then c else Int64.compare ps0 ps1

let ptime_span_to_mtime_span_ns (d, ps) = (* assert (d >= 0) *)
  let d_ns = Int64.(mul (of_int d) ns_count_in_day) in
  let ns = Int64.div ps 1000L in
  Int64.add d_ns ns

(* Timelines *)

type blocked_wait =
| Mtime of Action.Blocked.Value.t
| Ptime of ptime_d_ps * Action.Blocked.Value.t

module Deadline = struct
  type t = mtime_ns * blocked_wait
  let compare (t0, _) (t1, _) = mtime_compare t0 t1
end

module Deadlines = Synchronized_pqueue.MakeMin (Deadline)

type t = { gc_count : int Atomic.t; deadlines : Deadlines.t; }

let make () = { gc_count = Atomic.make 0; deadlines = Deadlines.make () }

let gc_threshold = 300
let gc_synced tl =
  let not_synced = function
  | _, Mtime blocked -> Action.Blocked.Value.is_not_synced blocked
  | _, Ptime (_, blocked) -> Action.Blocked.Value.is_not_synced blocked
  in
  Atomic.set tl.gc_count 0;
  Deadlines.keep ~sat:not_synced tl.deadlines

let maybe_gc_synced tl =
  if Atomic.get tl.gc_count > gc_threshold then gc_synced tl

let is_empty tl = gc_synced tl; Deadlines.is_empty tl.deadlines

let add_deadline deadline tl =
  Atomic.incr tl.gc_count; Deadlines.add deadline tl.deadlines

let add_mtime_deadline tl t blocked = add_deadline (t, Mtime blocked) tl
let add_ptime_deadline tl ~now ptime blocked =
  let posix_span = ptime_sub ptime now in (* assert (ptime > now) *)
  let mtime_span_ns = ptime_span_to_mtime_span_ns posix_span in
  let now = mtime_now_ns () in
  let mt = match mtime_add_span now mtime_span_ns with
  | None -> mtime_max_stamp (* well… *) | Some t -> t
  in
  add_deadline (mt, Ptime (ptime, blocked)) tl

let unblock_blocked_wait tl blocked_wait = match blocked_wait with
| Mtime blocked -> Action.Blocked.Value.synced_unblock_is_ours blocked
| Ptime (t, blocked) ->
    let now = ptime_now_d_ps () in
    if ptime_compare t now <= 0
    then Action.Blocked.Value.synced_unblock_is_ours blocked
    else (add_ptime_deadline tl ~now t blocked; false)

let mtime_deadline_expired t ~now = mtime_compare t now <= 0

let progress_to_next_deadline tl =
  let rec unblock ~did_unblock tl ~now =
    let expired (t, _) = mtime_deadline_expired t ~now in
    match Deadlines.pop_sat_or_peek_min ~sat:expired tl.deadlines with
    | None -> did_unblock, None
    | Some Either.Left (_, blocked_wait) ->
        Atomic.decr tl.gc_count;
        let unblocked = unblock_blocked_wait tl blocked_wait in
        let did_unblock = did_unblock || unblocked in
        unblock ~did_unblock tl ~now
    | Some Either.Right (t, _) ->
        let now = mtime_now_ns () in
        if mtime_deadline_expired t ~now
        then unblock ~did_unblock tl ~now else
        let next_deadline_in_ns = Int64.sub t now in
        did_unblock, Some next_deadline_in_ns
  in
  let ret = unblock ~did_unblock:false tl ~now:(mtime_now_ns ()) in
  maybe_gc_synced tl; ret

(* Domain local timeline

   Each domain may know about a timeline via domain local storage.
   This allows APIs calls in Mtime and Ptime to register deadlines
   on the appropriate timeline. *)

let nil = make ()
let err_timeline_not_set () = invalid_arg "Timeline not set"
let key : t Domain.DLS.key = Domain.DLS.new_key (fun () -> nil)
let set_domain_local tl = Domain.DLS.set key tl
let clear_domain_local () = set_domain_local nil
let get_domain_local () =
  let tl = Domain.DLS.get key in
  if Repr.phys_equal tl nil then err_timeline_not_set () else tl
