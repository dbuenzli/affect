(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect__base
open Affect__action.Private

let err_negative n = Printf.sprintf "negative capacity %d" n
let err_overflow cap = Printf.sprintf "overflowing the capacity of %d" cap

type blocked_acquire = Action.Blocked.Value.t
type value =
| Value of int
| Zero of blocked_acquire Persistent_queue.t

type t = { capacity : int; value : value Atomic.t;  }
let make n =
  if n < 0 then invalid_arg (err_negative n) else
  { capacity = n; value = Atomic.make (Value n) }

(* Acquire action *)

let acquire_poll s tag ~continue =
  let rec loop ~backoff s = match Atomic.get s.value with
  | Value n as seen ->
      let n = n - 1 in
      let value = if n = 0 then Zero Persistent_queue.empty else Value n in
      if Atomic.compare_and_set s.value seen value
      then Some (continue (Action.Result.Value tag))
      else loop ~backoff:(Atomic.Backoff.once backoff) s
  | Zero _ -> None
  in
  loop ~backoff:Atomic.Backoff.default s

let acquire_block s tag ~blocked:acquire =
  match Action.Blocked.exchange_waiting_to_claimed acquire with
  | Claimed -> assert false
  | Synced -> ()
  | Waiting -> (* Now decrement or block *)
      let rec loop ~backoff s acquire = match Atomic.get s.value with
      | Value n as seen ->
          let n = n - 1 in
          let value = if n = 0 then Zero Persistent_queue.empty else Value n in
          if Atomic.compare_and_set s.value seen value then
            begin
              Action.Blocked.Value.set_state acquire Synced;
              Action.Blocked.Value.tricky_unblock acquire;
            end
          else loop ~backoff:(Atomic.Backoff.once backoff) s acquire
      | Zero acquires as seen ->
          let and_keep = Action.Blocked.Value.is_not_synced in
          let acquires =
            Persistent_queue.add' acquire ~and_keep acquires
          in
          if Atomic.compare_and_set s.value seen (Zero acquires)
          then Action.Blocked.Value.set_state acquire Waiting
          else loop ~backoff:(Atomic.Backoff.once backoff) s acquire
      in
      let acquire = Action.Blocked.Value.make tag acquire in
      loop ~backoff:Atomic.Backoff.default s acquire

let acquire_meta = Action.Meta.make ~name:"Semaphore.acquire" ()

let acquire' s tag =
  let poll = acquire_poll s tag in
  let block = acquire_block s tag in
  Action.Primitive.make ~meta:acquire_meta ~poll ~block

let acquire s = Action.invoke (acquire' s ())

(* release *)

let release s =
  let rec loop ~backoff s = match Atomic.get s.value with
  | Value n as seen ->
      let n = n + 1 in
      if n > s.capacity then invalid_arg (err_overflow s.capacity) else
      if Atomic.compare_and_set s.value seen (Value n) then () else
      loop ~backoff:(Atomic.Backoff.once backoff) s
  | Zero acquires as seen ->
      match Persistent_queue.take acquires with
      | None, _empty ->
          if Atomic.compare_and_set s.value seen (Value 1) then () else
          loop ~backoff:(Atomic.Backoff.once backoff) s
      | Some acquire, acquires ->
          if Atomic.compare_and_set s.value seen (Zero acquires) then
            if Action.Blocked.Value.synced_unblock_is_ours acquire then () else
            loop ~backoff s (* our increment was not counted *)
          else
          loop ~backoff s
  in
  loop ~backoff:Atomic.Backoff.default s

(* bracket *)

let with_acquired s f =
  let finally () = release s in
  acquire s; Fun.protect ~finally f

(* Properties *)

let get_value s = match Atomic.get s.value with Value n -> n | Zero _ -> 0
let capacity s = s.capacity
