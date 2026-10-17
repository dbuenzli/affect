(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect
open Affect_unix
open Affect.Action.Private

(* N.B. it would perhas be nicer to try to express that as an action
   unblocker composed with the unix unblocker instead of having an
   asynchronous function call for handling the driver loop. *)

module Atomic =
  (* for Atomic.update, available in OCaml > 5.6 *)
  Affect__base.Atomic

module Fd = struct type t = Unix.file_descr let compare = Repr.compare end
module Fdmap = Map.Make (Fd)

type blocked_perform = Curl.curlCode Action.Blocked.t
type driver =
  { mutex : Mutex.t;
    mt : Curl.Multi.mt;
    stop : bool Atomic.t;
    performs : (Curl.t * blocked_perform) list Atomic.t;
    next_timeout : Mtime.t Atomic.t;
    fds : Curl.Multi.poll Fdmap.t Atomic.t;
    unblock_curl_next : Action.Blocked.Value.t option Atomic.t; }

let with_driver_mutex d f = Mutex.protect d.mutex f
let driver = Atomic.make None

(* Managing the list of ongoing transfers *)

let mem_perform d h = List.mem_assoc h (Atomic.get d.performs)
let add_perform d h b = Atomic.update (fun ps -> (h, b) :: ps) d.performs
let get_and_remove_perform d h =
  let b = List.assoc_opt h (Atomic.get d.performs) |> Option.get in
  Atomic.update (fun ps -> List.remove_assoc h ps) d.performs; b

let abort_perform d h =
  Curl.Multi.remove d.mt h;
  let blocked = get_and_remove_perform d h in
  let candidate = Action.Result.Value Curl.CURLE_ABORTED_BY_CALLBACK in
  Action.Blocked.synced_unblock ~candidate blocked

(* Action to unblock the driver blocked waiting for the next curl
   event. Unblocking happens by synchronizing the d.unblock_curl_next
   blocked value by calling the [unblock_curl_next] function. *)

let unblock_curl_next_action : (driver -> [> `Unblock] Action.t) =
  let meta = Action.Meta.make ~name:"curl-unblock" () in
  fun d ->
    let poll = Action.Primitive.poll_is_none in
    let block ~blocked =
      let v = Action.Blocked.Value.make `Unblock blocked in
      Atomic.set d.unblock_curl_next (Some v)
    in
    Action.Primitive.make ~meta ~poll ~block

let unblock_curl_next d = match Atomic.get d.unblock_curl_next with
| None -> () | Some b -> Action.Blocked.Value.synced_unblock b

(* Action to wait for the next event to drive curl *)

let add_fd_actions fds acc =
  let add_read fd acc = Unix.wait_readable fd (`Read fd) :: acc in
  let add_write fd acc = Unix.wait_writable fd (`Write fd) :: acc in
  let add_fd fd poll acc = match poll with
  | Curl.Multi.POLL_NONE | Curl.Multi.POLL_REMOVE -> assert false
  | Curl.Multi.POLL_IN -> add_read fd acc
  | Curl.Multi.POLL_OUT -> add_write fd acc
  | Curl.Multi.POLL_INOUT -> add_read fd (add_write fd acc)
  in
  Fdmap.fold add_fd fds acc

let curl_next_action d =
  let now = Mtime.now () and next_timeout = Atomic.get d.next_timeout in
  if Mtime.is_later now ~than:next_timeout then Action.always `Timeout else
  let timeout =
    if Mtime.equal next_timeout Mtime.max_stamp
    then Action.never
    else Mtime.wait_for' (Mtime.span next_timeout now) `Timeout
  in
  let unblock = unblock_curl_next_action d in
  Action.choose (add_fd_actions (Atomic.get d.fds) [timeout; unblock])

(* Driver loop *)

let driver_stop d =
  List.iter (abort_perform d) (List.map fst (Atomic.get d.performs));
  Curl.Multi.cleanup d.mt;
  Atomic.set driver None

let rec sync_finished d = match Curl.Multi.remove_finished d.mt with
| None -> ()
| Some (h, code) ->
    let blocked = get_and_remove_perform d h in
    let candidate = Action.Result.Value code in
    Action.Blocked.synced_unblock ~candidate blocked;
    sync_finished d

let rec driver_loop d =
  if Atomic.get d.stop then with_driver_mutex d (fun () -> driver_stop d) else
  let next_action = with_driver_mutex d (fun () -> curl_next_action d) in
  let next = Action.invoke next_action in
  begin try with_driver_mutex d @@ fun () ->
    let () = match next with
    | `Unblock -> ()
    | `Timeout ->
        Atomic.set d.next_timeout Mtime.max_stamp;
        Curl.Multi.action_timeout d.mt
    | `Read fd -> ignore (Curl.Multi.action d.mt fd EV_IN)
    | `Write fd -> ignore (Curl.Multi.action d.mt fd EV_OUT)
    in
    sync_finished d;
  with
  (* The error model of the curl bindings is utterly confusing *)
  | Failure s | Curl.Multi.Error s -> Fun.Async.trace "%s" s
  | Curl.Multi.CError (origin, _, msg) -> Fun.Async.trace "%s: %s" origin msg
  end;
  driver_loop d

(* Note we cannot perform effects (and thus use affect synchronization
   primitives) from cURL callbacks since those are invoked via C. Also
   bear in mind that the callbacks only get invoked when Curl.Multi.*
   function are so normally we should be holding driver lock when they occur. *)

let curl_socket_function d fd poll = match poll with
| Curl.Multi.POLL_NONE (* ? *) -> ()
| Curl.Multi.POLL_REMOVE -> Atomic.update (Fdmap.remove fd) d.fds
| poll -> Atomic.update (Fdmap.add fd poll) d.fds

let curl_timer_function d dur_ms =
  let t =
    if dur_ms = -1 then Mtime.max_stamp else
    let dur_ms = Mtime.Span.(dur_ms * ms) in
    Mtime.add_span (Mtime.now ()) dur_ms |> Option.get
  in
  Atomic.set d.next_timeout t

let init ?(opts = []) () =
  let mutex = Mutex.create () and mt = Curl.Multi.create ()
  and stop = Atomic.make false and performs = Atomic.make []
  and next_timeout = Atomic.make Mtime.max_stamp
  and fds = Atomic.make Fdmap.empty and unblock_curl_next = Atomic.make None in
  let d = { mutex; mt; stop; performs; next_timeout; fds; unblock_curl_next } in
  List.iter (Curl.Multi.setopt mt) opts;
  Curl.Multi.set_socket_function mt (curl_socket_function d);
  Curl.Multi.set_timer_function mt (curl_timer_function d);
  Atomic.set driver (Some d);
  Fun.Async.call_trap_exn (fun () -> driver_loop d);
  ()

let driver () = Option.get (Atomic.get driver)
let deinit () =
  let d = driver () in
  Atomic.set d.stop true; unblock_curl_next d

(* Perform action *)

let url_key = Action.Meta.Key.make ~pp_value:Format.pp_print_string ()

let perform_meta h =
  let url = Curl.get_effectiveurl h in
  let bindings = Action.Meta.[Binding (url_key, url)] in
  Action.Meta.make ~bindings ~name:"Affect_curl.perform" ()

let perform_block h ~blocked =
  let d = driver () in
  if mem_perform d h then invalid_arg "Curl.t value already performing" else
  with_driver_mutex d @@ fun () ->
  (* Note we have no shorcut if [blocked] already synchronized, we
     uncondionally perform the transfer. *)
  Curl.Multi.add d.mt h;
  add_perform d h blocked;
  unblock_curl_next d;
  ()

let perform' h =
  let poll = Action.Primitive.poll_is_none in
  let block = perform_block h in
  Action.Primitive.make ~meta:(perform_meta h) ~poll ~block

(* Transfers *)

let perform h = Action.invoke (perform' h)
let abort h =
  let d = driver () in
  with_driver_mutex d @@ fun () ->
  if mem_perform d h then (abort_perform d h; unblock_curl_next d)
