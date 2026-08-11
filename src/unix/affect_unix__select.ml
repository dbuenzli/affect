(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect__base
open Affect.Action.Private
open Affect_unix__fd

(* Note this is a toy, it might work on Windows though. *)

type blocked = Action.Blocked.Value.t
type blocked_state = { readable : blocked list; writable : blocked list; }

let unblock_fd rset wset fd st =
  let readable =
    if List.mem fd rset
    then (List.iter Action.Blocked.Value.synced_unblock st.readable; [])
    else List.filter Action.Blocked.Value.is_not_synced st.readable (* gc *)
  in
  let writable =
    if List.mem fd wset
    then (List.iter Action.Blocked.Value.synced_unblock st.writable; [])
    else List.filter Action.Blocked.Value.is_not_synced st.writable (* gc *)
  in
  if List.is_empty readable && List.is_empty writable then None else
  Some { readable; writable }

type t = { fds : blocked_state Fd.Synchronized_map.t; block_bypass : Flagfd.t }

let make () =
  let fds = Fd.Synchronized_map.make () and block_bypass = Flagfd.make () in
  { fds; block_bypass }

let dispose u = Flagfd.dispose u.block_bypass

let gc_fd fd st =
  let is_not_synced = Action.Blocked.Value.is_not_synced in
  let readable = List.filter is_not_synced st.readable in
  let writable = List.filter is_not_synced st.writable in
  if List.is_empty readable && List.is_empty writable then None else
  Some { readable; writable }

let gc_ebadf fd st =
  (* There's two reasons why we may get an EBADF.
     1. We were part of a choice that we did not win and that ended up with
        the fd being closed. In this case we don't error but simply prune
        out the synced blocks with [gc_fd]. This could be checked when we
        call [fd_sets], but we try optimistcally.
     2. There is a use after close error, in this case we try to throw an
        error in the blocked value if it hasn't synced yet. *)
  match gc_fd fd st with
  | None -> None
  | Some st as keep ->
      try Unix.set_nonblock fd (* hack to find closed fd *); keep with
      | Unix.Unix_error (EBADF, _, _) ->
          let bt = Printexc.get_raw_backtrace () in
          let exn op = Unix.Unix_error (EBADF, op, "") in
          let unblock exn (Action.Blocked.Value.V (_, b)) =
            let candidate = Action.Result.Exn (exn, bt) in
            Action.Blocked.synced_unblock ~candidate b
          in
          List.iter (unblock (exn "Unix.wait_readable")) st.readable;
          List.iter (unblock (exn "Unix.wait_writable")) st.writable;
          None

let gc_synced u = Fd.Synchronized_map.update_all gc_fd u.fds
let gc_ebadf u = Fd.Synchronized_map.update_all gc_ebadf u.fds
let is_empty u = gc_synced u; Fd.Synchronized_map.is_empty u.fds
let fd_sets u =
  let add fd st (rset, wset) =
    let rset = if List.is_empty st.readable then rset else fd :: rset in
    let wset = if List.is_empty st.writable then wset else fd :: wset in
    (rset, wset)
  in
  Fd.Synchronized_map.fold add u.fds ([], [])

let add_wait_readable u fd ~blocked =
  let add_read r = function
  | None -> Some { readable = [r]; writable = [] }
  | Some st -> Some { st with readable = r :: st.readable }
  in
  Fd.Synchronized_map.update fd (add_read blocked) u.fds

let add_wait_writable u fd ~blocked =
  let add_write w = function
  | None -> Some { readable = []; writable = [w] }
  | Some st -> Some { st with writable = w :: st.writable }
  in
  Fd.Synchronized_map.update fd (add_write blocked) u.fds

let rec unblock u ~timeout_ns =
  let timeout_s = match timeout_ns with
  | None -> -1.
  | Some dur_s -> Affect_unix__timeline.mtime_span_ns_to_float_s dur_s
  in
  let blocking = timeout_s <> 0. in
  let rset, wset = fd_sets u in
  let rset = Affect_unix__signal.syscall_block_bypass_fd :: rset in
  let rset = if blocking then Flagfd.fd u.block_bypass :: rset else rset in
  let did_unblock = match Unix.select rset wset [] timeout_s with
  | exception Unix.Unix_error (EBADF, _, _) ->
      gc_ebadf u; unblock u ~timeout_ns
  | exception Unix.Unix_error (EINTR, _, _) ->
      Affect_unix__signal.clear_syscall_block_bypass ();
      if blocking then Flagfd.clear u.block_bypass;
      false
  | rset, wset, _eset ->
      Affect_unix__signal.clear_syscall_block_bypass ();
      if blocking then Flagfd.clear u.block_bypass;
      (* XXX Except for performance there is no harm in doing the operation
         more than once if there is an atomic retry. But we should try to
         eventually avoid this. A first step would perhaps to do it atomically
         on each fd with an iter, we don't really need the whole batch to be
         consistent. It's still a good idea to go over all of them for
         gc, though we could move that to [fd_sets]. *)
      Fd.Synchronized_map.update_all (unblock_fd rset wset) u.fds;
      not (List.is_empty rset) || not (List.is_empty wset)
      (* XXX formally we may not have unblocked anything because we are not
         using [sync_unblock_is_ours] in [blocked_synced_unblock] or rather
         it's complicated given our simplistic datastructures to get it out.
         So we just use this which is good enough, there may be false
         positives (also the signal and block bypass may show up) but it's
         harmless. *)
  in
  did_unblock

let set_block_bypass u = Flagfd.set u.block_bypass

(* Domain local fd unblocker

   Each domain may know about an unblocker via domain local storage.
   This allows APIs calls in Unix to register readability and writability
   request on the appropriate unblocker. Note that the current scheme
   could experience high-contention, one idea would be to have
   one synchronized map per domain and the domain that calls [unblocks]
   merges them before calling select. *)

let nil = let nil = make () in dispose nil; nil
let err_unblocker_not_set () = invalid_arg "Fd unblocker not set"
let key : t Domain.DLS.key = Domain.DLS.new_key (fun () -> nil)
let set_domain_local u = Domain.DLS.set key u
let clear_domain_local () = set_domain_local nil
let get_domain_local () =
  let u = Domain.DLS.get key in
  if Repr.phys_equal u nil then err_unblocker_not_set () else u
