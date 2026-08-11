(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect__base
open Affect__action.Private

module Drop = struct
  type 'a blocked_put = 'a * Action.Blocked.Value.t
  type 'a blocked_take = 'a Action.Blocked.t
  type 'a state =
  | Full of 'a * 'a blocked_put Persistent_queue.t
  | Empty of 'a blocked_take Persistent_queue.t

  (* XXX Atomic over state is challenging, another intermediate state
     is likely needed for take, let's go with a spinlock for now *)

  type 'a t =
    { lock : Spin_lock.t;
      mutable state : 'a state; }

  let empty_state = Empty Persistent_queue.empty
  let make () =
    let lock = Spin_lock.make () and state = empty_state in
    { lock; state }

  let is_empty cell =
    Spin_lock.protect cell.lock @@ fun () -> match cell.state with
    | Empty _ -> true | Full _ -> false

  (* put *)

  let put_poll cell v tag ~continue =
    Spin_lock.protect cell.lock @@ fun () -> match cell.state with
    | Full _ -> None
    | Empty takes ->
        let rec put cell v takes = match Persistent_queue.take takes with
        | None, _empty -> cell.state <- Full (v, Persistent_queue.empty);
        | Some take, takes ->
            let candidate = Action.Result.Value v in
            if Action.Blocked.synced_unblock_is_ours ~candidate take
            then cell.state <- Empty takes
            else put cell v takes
        in
        put cell v takes;
        Some (continue (Action.Result.Value tag))

  let put_block cell v tag ~blocked:put =
    Spin_lock.protect cell.lock @@ fun () ->
    let put = Action.Blocked.Value.make tag put in
    match cell.state with
    | Full (pv, puts) ->
        let and_keep (_, b) = Action.Blocked.Value.is_not_synced b in
        let puts = Persistent_queue.add' (v, put) ~and_keep puts in
        cell.state <- Full (pv, puts)
    | Empty takes ->
        let rec loop cell v put takes =
          let is_not_me = Action.Blocked.Value.is_not put in
          match Persistent_queue.take' ~sat:is_not_me takes with
          | None, _empty ->
              (* If there was only us the sync discards it *)
              begin match Action.Blocked.Value.exchange_waiting_to_claimed put
              with
              | Claimed -> assert false
              | Synced -> ()
              | Waiting ->
                  cell.state <- Full (v, Persistent_queue.empty);
                  Action.Blocked.Value.set_state put Synced;
                  Action.Blocked.Value.tricky_unblock put;
              end
          | Some take, takes ->
              if Action.Blocked.is_synced take then loop cell v put takes else
              let rec try_this_take cell v put take =
                begin match Action.Blocked.Value.exchange_waiting_to_claimed put
                with
                | Claimed -> assert false
                | Synced -> cell.state <- Empty takes (* we may have gc'd *)
                | Waiting ->
                    match Action.Blocked.exchange_waiting_to_synced take with
                    | Claimed ->
                        Action.Blocked.Value.set_state put Waiting;
                        try_this_take cell v put take
                    | Synced ->
                        Action.Blocked.Value.set_state put Waiting;
                        loop cell v put takes
                    | Waiting ->
                        cell.state <- Empty takes;
                        Action.Blocked.Value.set_state put Synced;
                        Action.Blocked.tricky_unblock take
                          (Action.Result.Value v);
                        Action.Blocked.Value.tricky_unblock put;
                end
              in
              try_this_take cell v put take
        in
        loop cell v put takes

  let put_meta = Action.Meta.make ~name:"Cell.Drop.put" ()
  let put' cell v tag =
    let poll = put_poll cell v tag and block = put_block cell v tag in
    Action.Primitive.make ~meta:put_meta ~poll ~block

  let put cell v = Action.invoke (put' cell v ())

  (* take *)

  let take_poll cell ~continue =
    Spin_lock.protect cell.lock @@ fun () -> match cell.state with
    | Empty _ -> None
    | Full (v, puts) ->
        let rec try_put cell puts = match Persistent_queue.take puts with
        | None, _empty -> cell.state <- empty_state;
        | Some (v, put), puts ->
            if Action.Blocked.Value.synced_unblock_is_ours put
            then cell.state <- Full (v, puts)
            else try_put cell puts
        in
        try_put cell puts;
        Some (continue (Action.Result.Value v))

  let take_block cell ~blocked:take =
    Spin_lock.protect cell.lock @@ fun () -> match cell.state with
    | Empty takes ->
        let and_keep = Action.Blocked.is_not_synced in
        let takes = Persistent_queue.add' take ~and_keep takes in
        cell.state <- Empty takes
    | Full (v, puts) ->
        match Action.Blocked.exchange_waiting_to_claimed take with
        | Claimed -> assert false
        | Synced -> ()
        | Waiting ->
            let rec try_put cell take puts =
              let is_not_me (_, b) = Action.Blocked.Value.is_not b take in
              match Persistent_queue.take' ~sat:is_not_me puts with
              | None, _empty -> cell.state <- empty_state;
              | Some (v, put), puts ->
                  if Action.Blocked.Value.synced_unblock_is_ours put
                  then cell.state <- Full (v, puts)
                  else try_put cell take puts
            in
            Action.Blocked.set_state take Synced;
            try_put cell take puts;
            Action.Blocked.tricky_unblock take (Action.Result.Value v)

  let take_meta = Action.Meta.make ~name:"Cell.Drop.take" ()
  let take' cell =
    let poll = take_poll cell and block = take_block cell in
    Action.Primitive.make ~meta:take_meta ~poll ~block

  let take cell = Action.invoke (take' cell)
end

module Lazy = struct
  type 'a blocked_force = 'a Action.Blocked.t
  type 'a state =
  | Suspended of (unit -> 'a)
  | Forcing of 'a blocked_force list
  | Forced of 'a Action.Result.t

  type 'a t = 'a state Atomic.t

  let make f = Atomic.make (Suspended f)
  let from_val v = Atomic.make (Forced (Action.Result.Value v))
  let from_exn e bt = Atomic.make (Forced (Action.Result.Exn (e, bt)))
  let is_forced cell = match Atomic.get cell with Forced _ -> true | _ -> false
  let is_val cell = match Atomic.get cell with
  | Forced (Action.Result.Value _) -> true | _ -> false

  (* force *)

  let set cell result ~continue =
    let rec loop ~backoff cell result ~continue = match Atomic.get cell with
    | Forced _ | Suspended _ -> assert false
    | Forcing forces as seen ->
        if Atomic.compare_and_set cell seen (Forced result) then
          let candidate = result and forces = List.rev forces in
          List.iter (Action.Blocked.synced_unblock ~candidate) forces;
          Some (continue result)
        else
        loop ~backoff:(Atomic.Backoff.once backoff) cell result ~continue
    in
    loop ~backoff:Atomic.Backoff.default cell result ~continue

  let force_poll cell ~continue =
    let rec loop ~backoff cell ~continue = match Atomic.get cell with
    | Forced result -> Some (continue result)
    | Forcing _ -> None
    | Suspended f as seen ->
        if Atomic.compare_and_set cell seen (Forcing [])
        then set cell (Action.Result.of_fun_run f) ~continue
        else loop ~backoff:(Atomic.Backoff.once backoff) cell ~continue
    in
    loop ~backoff:Atomic.Backoff.default cell ~continue

  let force_block cell ~blocked:force =
    let rec loop ~backoff cell force = match Atomic.get cell with
    | Suspended _ -> assert false (* by protocol [poll] has been called *)
    | Forced result -> Action.Blocked.synced_unblock ~candidate:result force
    | Forcing forces as seen ->
        let keep = Action.Blocked.is_not_synced in
        let forces = List.filter keep (force :: forces) in
        if Atomic.compare_and_set cell seen (Forcing forces) then () else
        loop ~backoff:(Atomic.Backoff.once backoff) cell force
    in
    loop ~backoff:Atomic.Backoff.default cell force

  let force_meta = Action.Meta.make ~name:"Cell.Lazy.force" ()
  let force' cell =
    let poll = force_poll cell and block = force_block cell in
    Action.Primitive.make ~meta:force_meta ~poll ~block

  let force cell = match Atomic.get cell with
  | Forced result -> Action.Result.return result
  | _ -> Action.invoke (force' cell)
end

module Once = struct
  type 'a blocked_get = 'a Action.Blocked.t
  type 'a state = Unset of 'a blocked_get list | Set of 'a
  type 'a t = 'a state Atomic.t

  let make () = Atomic.make (Unset [])
  let from_val v = Atomic.make (Set v)
  let is_set cell = match Atomic.get cell with Set _ -> true | _ -> false
  let try_set_is_ours cell v =
    let rec loop ~backoff cell v = match Atomic.get cell with
    | Set _ -> false
    | Unset gets as seen ->
        if Atomic.compare_and_set cell seen (Set v) then
          let candidate = Action.Result.Value v and gets = List.rev gets in
          List.iter (Action.Blocked.synced_unblock ~candidate) gets;
          true
        else
        loop ~backoff:(Atomic.Backoff.once backoff) cell v
    in
    loop ~backoff:Atomic.Backoff.default cell v

  let try_set cell v = ignore (try_set_is_ours cell v)

  (* get *)

  let get_poll cell ~continue = match Atomic.get cell with
  | Set v -> Some (continue (Action.Result.Value v))
  | Unset _ -> None

  let get_block cell ~blocked:get =
    let rec loop ~backoff cell get = match Atomic.get cell with
    | Set v ->
        let candidate = Action.Result.Value v in
        Action.Blocked.synced_unblock ~candidate get
    | Unset gets as seen ->
        let keep = Action.Blocked.is_not_synced in
        let gets = List.filter keep (get :: gets) in
        if Atomic.compare_and_set cell seen (Unset gets) then () else
        loop ~backoff:(Atomic.Backoff.once backoff) cell get
    in
    loop ~backoff:Atomic.Backoff.default cell get

  let get_meta = Action.Meta.make ~name:"Cell.Once.get" ()
  let get' cell =
    let poll = get_poll cell and block = get_block cell in
    Action.Primitive.make ~meta:get_meta ~poll ~block

  let get cell = match Atomic.get cell with
  | Set v -> v
  | Unset _ -> Action.invoke (get' cell)
end
