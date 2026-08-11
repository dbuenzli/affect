(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect__base
open Affect__action.Private
open Affect__async_fun.Private

(* Execution context identifiers *)

type thread_id = int
let current_thread_id () = Thread.id (Thread.self ())

type domain_id = int
let current_domain_id () = (Domain.self () :> int)

type exec_id = { thread_id : thread_id; sup : domain_id; index : int }
let current_exec_id () =
  { thread_id = current_thread_id (); sup = current_domain_id (); index = -1;  }

(* Traces *)

type payload =
| Action_block of Async_fun.Call.id * Action.Meta.t
| Action_unblock of Async_fun.Call.id * Action.Meta.t
| Async_fun_call of { parent : Async_fun.Call.id; id : Async_fun.Call.id }
| Async_fun_continue of Async_fun.Call.id
| Async_fun_yield of Async_fun.Call.id
| Async_fun_return of Async_fun.Call.id
| Async_fun_user of Async_fun.Call.id * string
| Domain_start
| Domain_sleep
| Domain_unblocker of { block : bool }
| Domain_resume
| Domain_panic of string
| Domain_stop

type t = exec_id * payload

let is_domain (_, payload) = match payload with
| Domain_start | Domain_sleep | Domain_unblocker _ | Domain_resume
| Domain_stop -> true
| _ -> false

let is_fun t = not (is_domain t)
let is_user (_, p) = match p with Async_fun_user _ -> true | _ -> false

(* Formatting *)

let call_none_id = Async_fun.Call.none_id

let pf = Format.fprintf
let pp_lines ppf msg =
  let lines = (String.split_on_char '\n' msg) in
  Format.(pp_print_list Format.pp_print_string) ppf lines

let pp_call_id = Async_fun.Call.pp_id

let pp_exec_id ppf eid =
  (* The special case of [eid.sup = 0] is to have less noise if you just
     Fun.Async.main in the program and you are happy with that. *)
  if eid.index = -1 then pf ppf "%d.τ%02x" eid.sup eid.thread_id else
  if eid.sup = 0 then pf ppf "%02d" eid.index else
  pf ppf "%d.%02d" eid.sup eid.index

let pp_exec_trace ppf eid = pf ppf "[%a      ]" pp_exec_id eid
let pp_fun_trace ppf (eid, id) = pf ppf "[%a %a]" pp_exec_id eid pp_call_id id
let pp_block_meta ppf m = Action.Meta.pp ppf m

let pp ppf (eid, trace) = match trace with
| Action_block (id, meta) ->
    pf ppf "%a @<0>\x1B[1mblock@<0>\x1B[0m on @[%a@]"
      pp_fun_trace (eid, id) pp_block_meta meta
| Action_unblock (id, meta) ->
    pf ppf "%a @<0>\x1B[1munblock@<0>\x1B[0m @[%a@] from @[%a@]"
      pp_exec_trace eid pp_call_id id pp_block_meta meta
| Async_fun_call { parent; id } ->
    if parent = call_none_id
    then pf ppf "%a root function" pp_fun_trace (eid, id)
    else pf ppf "%a calls %a" pp_fun_trace (eid, parent) pp_call_id id
| Async_fun_continue id -> pf ppf "%a continue" pp_fun_trace (eid, id)
| Async_fun_yield id -> pf ppf "%a yield" pp_fun_trace (eid, id)
| Async_fun_return id ->
    pf ppf "%a @<0>\x1B[1mreturn@<0>\x1B[0m" pp_fun_trace (eid, id)
| Async_fun_user (id, msg) -> pf ppf "%a %s" pp_fun_trace (eid, id) msg
| Domain_start -> pf ppf "%a start" pp_exec_trace eid
| Domain_sleep -> pf ppf "%a sleep" pp_exec_trace eid
| Domain_unblocker { block } ->
    if block
    then pf ppf "%a @<0>\x1B[1mblock@<0>\x1B[0m on unblocker" pp_exec_trace eid
    else pf ppf "%a poll unblocker" pp_exec_trace eid
| Domain_resume -> pf ppf "%a resume" pp_exec_trace eid
| Domain_panic msg ->
    pf ppf "@[<v>%a panic@,%a@]" pp_exec_trace eid pp_lines msg
| Domain_stop -> pf ppf "%a stop" pp_exec_trace eid

(* Reporters *)

type reporter = t -> unit

let default_reporter = ignore
let format_reporter ppf tr = pf (ppf ()) "@[%a@]@." pp tr
let stderr_reporter = format_reporter (Format.get_err_formatter)
let keep sat report = fun t -> if sat t then report t
let only_fun report = keep is_fun report
let only_user report = keep is_user report

let reporter' : reporter Atomic.t = Atomic.make default_reporter
let reporter () = Atomic.get reporter'
let set_reporter report = Atomic.set reporter' report
let exchange_reporter report = Atomic.exchange reporter' report
let with_reporter report f =
  let old = exchange_reporter report in
  Fun.protect ~finally:(fun () -> set_reporter old) f

module Delayed = struct
  type nonrec t = t list Atomic.t
  let make () = Atomic.make []
  let reporter ts = fun t -> Atomic.update (fun ts -> t :: ts) ts
  let clear ts = Atomic.update (fun _ -> []) ts
  let traces ts = List.rev (Atomic.get ts)

  let pp ppf ts =
    Format.fprintf ppf "@[<v>%a@]" (Format.pp_print_list pp) (traces ts)

  let prerr ts =
    Format.fprintf (Format.get_err_formatter ()) "@[%a@]@." pp ts
end

(* Reporting *)

let report t = try (Atomic.get reporter') t with
| exn when not (Exn.is_runtime_system exn) ->
    let bt = Printexc.get_raw_backtrace () in
    Exn.trap exn bt
