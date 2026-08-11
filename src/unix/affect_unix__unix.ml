(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect
open Affect__base
open Affect.Action.Private

module Signal = Affect_unix__signal

(* Unblocking the cooperants *)

module Fd_unblocker =
  (* XXX eventually plug that that with system dependent fd unblocker
     modules. *)
  Affect_unix__select

module Unblocker = struct
  type t =
    { timeline : Affect_unix__timeline.t;
      signal_unblocker : Signal.Unblocker.t;
      fd_unblocker : Fd_unblocker.t; }

  let make () =
    let timeline = Affect_unix__timeline.make () in
    let signal_unblocker = Signal.Unblocker.make () in
    let fd_unblocker = Fd_unblocker.make () in
    { timeline; signal_unblocker; fd_unblocker }

  let nonblock = Some 0L
  let unblock u ~block =
    let timer_did_unblock, timeout_ns =
      Affect_unix__timeline.progress_to_next_deadline u.timeline
    in
    let signal_did_unblock = Signal.Unblocker.unblock u.signal_unblocker in
    let did_unblock = timer_did_unblock || signal_did_unblock in
    let timeout_ns =
      if not block || did_unblock then nonblock else timeout_ns
    in
    let fd_did_unblock = Fd_unblocker.unblock u.fd_unblocker ~timeout_ns in
    did_unblock || fd_did_unblock

  let set_block_bypass u () = Fd_unblocker.set_block_bypass u.fd_unblocker

  let domain_local u =
    let unblock = unblock u in
    let set_block_bypass = set_block_bypass u in
    Action.Unblocker.Domain_local.make ~unblock ~set_block_bypass ()

  let init u = Signal.Unblocker.register u.signal_unblocker
  let deinit u =
    Signal.Unblocker.unregister u.signal_unblocker;
    Fd_unblocker.dispose u.fd_unblocker

  let domain_install u =
    Affect_unix__timeline.set_domain_local u.timeline;
    Signal.Unblocker.set_domain_local u.signal_unblocker;
    Fd_unblocker.set_domain_local u.fd_unblocker

  let domain_uninstall _u =
    Affect_unix__timeline.clear_domain_local ();
    Signal.Unblocker.clear_domain_local ();
    Fd_unblocker.clear_domain_local ()
end

let unblocker () =
  (* We share a single unblocker among all domain *)
  let u = Unblocker.make () in
  let local = Unblocker.domain_local u in
  let init ~domain_count:_ = Unblocker.init u in
  let domain_local _index = local in
  let domain_install _local = Unblocker.domain_install u in
  let domain_uninstall _local = Unblocker.domain_uninstall u in
  let deinit () = Unblocker.deinit u in
  Action.Unblocker.make
    ~init ~domain_local ~domain_install ~domain_uninstall ~deinit ()

let main
    ?(sigpipe = Signal.Ignore) ?domain_spawn ?domain_count ?schedule ?handler f
  =
  Signal.set_and_restore Sys.sigpipe sigpipe @@ fun () ->
  let unblocker = unblocker () in
  Fun.Async.main ~unblocker ?domain_spawn ?domain_count ?schedule ?handler f

(* Exending and overriding Unix *)

include Unix

(* File descriptor actions *)

let wait_readable_meta = Action.Meta.make ~name:"Unix.wait_readable" ()
let wait_readable fd tag =
  let poll = Action.Primitive.poll_is_none in
  let block ~blocked =
    let blocked = Action.Blocked.Value.make tag blocked in
    let unblocker = Fd_unblocker.get_domain_local () in
    Fd_unblocker.add_wait_readable unblocker fd ~blocked
  in
  Action.Primitive.make ~meta:wait_readable_meta ~poll ~block

let wait_writable_meta = Action.Meta.make ~name:"Unix.wait_writable" ()
let wait_writable fd tag =
  let poll = Action.Primitive.poll_is_none in
  let block ~blocked =
    let blocked = Action.Blocked.Value.make tag blocked in
    let unblocker = Fd_unblocker.get_domain_local () in
    Fd_unblocker.add_wait_writable unblocker fd ~blocked
  in
  Action.Primitive.make ~meta:wait_writable_meta ~poll ~block

(* File descriptor operations *)

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

(* Sockets *)

let socket ?(cloexec = true) dom typ proto =
  let fd = Unix.socket ~cloexec dom typ proto in
  Unix.set_nonblock fd; fd

let rec accept ?cloexec fd = try Unix.accept ?cloexec fd with
| Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) ->
    Action.invoke (wait_readable fd ()); accept ?cloexec fd

let rec connect fd addr = try Unix.connect fd addr with
| Unix.Unix_error (EINPROGRESS, _, _) ->
    Action.invoke (wait_writable fd ());
    match Unix.getsockopt_error fd with
    | Some error -> raise (Unix.Unix_error (error, "connect", ""))
    | None -> ()

(* Reads *)

let rec read fd buf first length = try Unix.read fd buf first length with
| Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) ->
    Action.invoke (wait_readable fd ()); read fd buf first length

let rec read_bigarray fd buf first length =
  try Unix.read_bigarray fd buf first length with
  | Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) ->
      Action.invoke (wait_readable fd ()); read_bigarray fd buf first length

(* Writes *)

let rec write fd buf first length =
  try Unix.write fd buf first length with
  | Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) ->
      Action.invoke (wait_writable fd ()); write fd buf first length

let rec write_bigarray fd buf first length =
  try Unix.write_bigarray fd buf first length with
  | Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) ->
      Action.invoke (wait_writable fd ()); write_bigarray fd buf first length

let rec write_substring fd buf first length =
  try Unix.write_substring fd buf first length with
  | Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) ->
      Action.invoke (wait_writable fd ()); write_substring fd buf first length

let rec single_write fd buf first length =
  try Unix.single_write fd buf first length with
  | Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) ->
      Action.invoke (wait_writable fd ()); single_write fd buf first length

let rec single_write_bigarray fd buf first length =
  try Unix.single_write_bigarray fd buf first length with
  | Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) ->
      Action.invoke (wait_writable fd ());
      single_write_bigarray fd buf first length

let rec single_write_substring fd buf first length =
  try Unix.single_write_substring fd buf first length with
  | Unix.Unix_error ((EWOULDBLOCK | EAGAIN), _, _) ->
      Action.invoke (wait_writable fd ());
      single_write_substring fd buf first length
