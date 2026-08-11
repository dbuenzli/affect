(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect
open Affect__base
open Affect.Action.Private
open Affect_unix__fd

(* Signals *)

module T = struct type t = Sys.signal let compare = Int.compare end
module Map = Map.Make (T)
module Synchronized_map = Synchronized_map.Make (Map)
let pp ppf signal = Format.pp_print_string ppf (Sys.signal_to_string signal)

(* Unblocking signal waiters *)

module Unblocker = struct
  type blocked_wait_signal = Action.Blocked.Value.t
  type t =
    { gc_count : int Atomic.t;
      (* See [handle_waiters] to understand why we have this [ready] field. *)
      ready : blocked_wait_signal list list Atomic.t;
      blocked : blocked_wait_signal list Synchronized_map.t }

  let make () =
    let gc_count = Atomic.make 0 and ready = Atomic.make [] in
    let blocked = Synchronized_map.make () in
    { gc_count; ready; blocked }

  let gc_threshold = 300
  let gc_synced u =
    let keep_not_synced _sg bs =
      let bs = List.filter Action.Blocked.Value.is_not_synced bs in
      if List.is_empty bs then None else Some bs
    in
    Atomic.set u.gc_count 0;
    Synchronized_map.update_all keep_not_synced u.blocked

  let maybe_gc_synced u =
    if Atomic.get u.gc_count > gc_threshold then gc_synced u

  let is_empty u =
    gc_synced u;
    Synchronized_map.is_empty u.blocked && List.is_empty (Atomic.get u.ready)

  let has_blocked_after_gc_synced u = not (is_empty u)

  let add_wait ~signal ~blocked u =
    Atomic.incr u.gc_count;
    Synchronized_map.add_to_list signal blocked u.blocked

  let ready_to_unblock ~signal u =
    match Synchronized_map.find_and_remove signal u.blocked with
    | None -> ()
    | Some bs -> Atomic.update (List.cons bs) u.ready

  let get_ready_and_clear u = Atomic.fold_update (fun l -> l, []) u.ready
  let unblock u =
    let unblock_list did_unblock b =
      Atomic.decr u.gc_count;
      Action.Blocked.Value.synced_unblock_is_ours b || did_unblock
    in
    let unblock_list_list did_unblock l =
      List.fold_left unblock_list did_unblock l
    in
    let did_unblock =
      let ready = get_ready_and_clear u in
      List.fold_left unblock_list_list false ready
    in
    maybe_gc_synced u; did_unblock

  (* Domain local signal unblocker

     Each domain may know about a signal unblocker via domain local storage.
     This allows APIs calls in Unix.Singal to register signal waits on
     on the appropriate unblocker. *)

  let nil = make ()
  let err_unblocker_not_set () = invalid_arg "Signal unblocker not set"
  let key : t Domain.DLS.key = Domain.DLS.new_key (fun () -> nil)
  let set_domain_local u = Domain.DLS.set key u
  let clear_domain_local () = set_domain_local nil
  let get_domain_local () =
    let u = Domain.DLS.get key in
    if Repr.phys_equal u nil then err_unblocker_not_set () else u

  (* Due to the global nature of signal handling we maintain a list of all
     existing signal unblockers. This list is updated when
     Unix.unblocker values install and deinstall. The list is consluted
     by signal handlers with [Waiters] handlers. *)

  let unblockers : t list Atomic.t = Atomic.make []
  let list () = Atomic.get unblockers
  let register u = Atomic.update (List.cons u) unblockers
  let unregister u = Atomic.update (List.filter (fun v -> v != u)) unblockers
end

let wait_block ~signal tag ~blocked =
  if Action.Blocked.is_synced blocked then () else
  let blocked = Action.Blocked.Value.make tag blocked in
  Unblocker.add_wait ~signal ~blocked (Unblocker.get_domain_local ())

let wait_key = Action.Meta.Key.make ~pp_value:pp ()
let wait_meta signal =
  let bindings = Action.Meta.[Binding (wait_key, signal)] in
  Action.Meta.make ~name:"Unix.Signal.wait" ~bindings ()

let wait' signal tag =
  let poll = Action.Primitive.poll_is_none in
  let block = wait_block ~signal tag in
  Action.Primitive.make ~meta:(wait_meta signal) ~poll ~block

let wait signal = Action.invoke (wait' signal ())
let wait_any signals =
  let waits = List.map (fun signal -> wait' signal signal) signals in
  Action.invoke (Action.choose waits)

(* Handling *)

type handler = Default | Ignore | Fun of (Sys.signal -> unit) | Waiters

(* We use a socket in non-blocking mode (see Flagfd) to unblock the
   system call used to unblock fds since EINTR is only triggered on
   the thread that handles the signal. Things we would also like to
   avoid are a dedicated thread for signals and complex signal masking
   setups. Hopefully this works regardless of all that. This should
   work if multiple domains are blocked at the same time. *)

let syscall_block_bypass = Flagfd.make ()
let clear_syscall_block_bypass () = Flagfd.clear syscall_block_bypass
let syscall_block_bypass_fd = Flagfd.fd syscall_block_bypass

let handle_waiters signal =
  (* Note we cannot run the unblockers in the signal handler because it may
     be executed by one of our sleeping domain which is holding it's
     [Affect__scheduler.Local_domain.state_mutex] lock. Since running the
     unblockers schedules work with [Affect__scheduler.schedule_ready] and that
     the latter tries to wakeup a sleeping domain and thus take the state lock,
     if it's the same domain that is woken up we get a deadlock and a nice
     Sys_error("Mutex.lock: Resource deadlock avoided") from OCaml. *)
  List.iter (Unblocker.ready_to_unblock ~signal) (Unblocker.list ());
  Flagfd.set syscall_block_bypass

let behaviour_of_handler = function
| Default -> Sys.Signal_default
| Ignore -> Sys.Signal_ignore
| Fun f -> Sys.Signal_handle f
| Waiters -> Sys.Signal_handle handle_waiters

let set signal h = Sys.set_signal signal (behaviour_of_handler h)
let set_and_restore signal h f =
  let old = Sys.signal signal (behaviour_of_handler h) in
  let finally () = Sys.set_signal signal old in
  Fun.protect ~finally f
