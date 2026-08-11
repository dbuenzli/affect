(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect__base
open Affect__action.Private

(* The Parallel concurrent ML paper <https://doi.org/10.1145/1596550.1596588>
   is useful to understand better what is going on here.

   This implementation differs though. We follow the guile implementation
   which uses two atomics on queues instead of a spinlock to synchronize
   access to the two queues at the same time. This means that in the
   blocking phase we need to publish operations on the queues before we try
   to synchronize and we must peek the queues, not take and possibly untake,
   otherwise we may end up in states where matching actions end up
   missing each other.

   This should perhaps still be benchmarked one day w.r.t. spin lock protected
   queues. *)

type 'a blocked_offer = 'a * Action.Blocked.Value.t
type 'a blocked_take = 'a Action.Blocked.t
type 'a t =
  { offers : 'a blocked_offer Synchronized_queue.t;
    takes : 'a blocked_take Synchronized_queue.t; }

let make () =
  { offers = Synchronized_queue.make ();
    takes = Synchronized_queue.make (); }

let discard v ~seen q = (* We don't care if the state changed *)
  ignore (Synchronized_queue.try_drop' ~sat:(Repr.phys_equal v) ~seen q)

(* offer *)

let rec offer_poll p v tag ~continue =
  match Synchronized_queue.take p.takes with
  | None -> None
  | Some take ->
      let candidate = Action.Result.Value v in
      if Action.Blocked.synced_unblock_is_ours ~candidate take
      then Some (continue (Action.Result.Value tag))
      else offer_poll p v tag ~continue

let offer_block p v tag ~blocked:offer =
  let offer = Action.Blocked.Value.make tag offer in
  let and_keep (_, b) = Action.Blocked.Value.is_not_synced b in
  Synchronized_queue.add' (v, offer) ~and_keep p.offers;
  let rec loop p v offer =
    let is_not_me = Action.Blocked.Value.is_not offer in
    match Synchronized_queue.peek' ~sat:is_not_me p.takes with
    | None, _seen -> ()
    | Some take, seen ->
        if Action.Blocked.is_synced take
        then (discard take ~seen p.takes; loop p v offer) else
        let rec try_this_take ~backoff p v offer take seen =
          match Action.Blocked.Value.exchange_waiting_to_claimed offer with
          | Claimed -> assert false
          | Synced -> ()
          | Waiting ->
              match Action.Blocked.exchange_waiting_to_synced take with
              | Waiting ->
                  discard take ~seen p.takes;
                  Action.Blocked.Value.set_state offer Synced;
                  Action.Blocked.tricky_unblock take (Action.Result.Value v);
                  Action.Blocked.Value.tricky_unblock offer;
              | Synced ->
                  discard take ~seen p.takes;
                  Action.Blocked.Value.set_state offer Waiting;
                  loop p v offer
              | Claimed ->
                  Action.Blocked.Value.set_state offer Waiting;
                  try_this_take ~backoff:(Atomic.Backoff.once backoff)
                    p v offer take seen
        in
        try_this_take ~backoff:Atomic.Backoff.default p v offer take seen
  in
  loop p v offer

let offer_meta = Action.Meta.make ~name:"Port.offer" ()
let offer' p v tag =
  let poll = offer_poll p v tag and block = offer_block p v tag in
  Action.Primitive.make ~meta:offer_meta ~poll ~block

let offer p v = Action.invoke (offer' p v ())

(* take *)

let rec take_poll p ~continue = match Synchronized_queue.take p.offers with
| None -> None
| Some (v, offer) ->
    if Action.Blocked.Value.synced_unblock_is_ours offer
    then Some (continue (Action.Result.Value v))
    else take_poll p ~continue

let take_block p ~blocked:take =
  let and_keep = Action.Blocked.is_not_synced in
  Synchronized_queue.add' take ~and_keep p.takes;
  let rec loop p take =
    let is_not_me (_, b) = Action.Blocked.Value.is_not b take in
    match Synchronized_queue.peek' ~sat:is_not_me p.offers with
    | None, _seen -> ()
    | Some (v, offer as o), seen ->
        if Action.Blocked.Value.is_synced offer
        then (discard o ~seen p.offers; loop p take) else
        let rec try_this_offer ~backoff p take ((v, offer) as o) seen =
          match Action.Blocked.exchange_waiting_to_claimed take with
          | Claimed -> assert false
          | Synced -> ()
          | Waiting ->
              match Action.Blocked.Value.exchange_waiting_to_synced offer with
              | Waiting ->
                  discard o ~seen p.offers;
                  Action.Blocked.set_state take Synced;
                  Action.Blocked.Value.tricky_unblock offer;
                  Action.Blocked.tricky_unblock take (Action.Result.Value v)
              | Synced ->
                  discard o ~seen p.offers;
                  Action.Blocked.set_state take Waiting;
                  loop p take
              | Claimed ->
                  Action.Blocked.set_state take Waiting;
                  try_this_offer ~backoff:(Atomic.Backoff.once backoff)
                    p take o seen
        in
        try_this_offer ~backoff:Atomic.Backoff.default p take o seen
  in
  loop p take

let take_meta = Action.Meta.make ~name:"Port.take" ()
let take' p =
  let poll = take_poll p and block = take_block p in
  Action.Primitive.make ~meta:take_meta ~poll ~block

let take p = Action.invoke (take' p)
