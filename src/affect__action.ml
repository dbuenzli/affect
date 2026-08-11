(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect__base

module Result = struct
  type 'a t = Value of 'a | Exn of exn * Printexc.raw_backtrace
  let none =
    let f, l, c, _ = __POS__ in
    Exn (Assert_failure (f, l, c), Printexc.get_callstack 0)

  let of_fun_run f = match f () with
  | v -> Value v
  | exception exn when not (Exn.is_runtime_system exn) ->
      let bt = Printexc.get_raw_backtrace () in
      Exn (exn, bt)

  let return = function
  | Value v -> v
  | Exn (exn, bt) -> Printexc.raise_with_backtrace exn bt

  let deep_continue k = function
  | Value v -> Effect.Deep.continue k v
  | Exn (exn, bt) -> Effect.Deep.discontinue_with_backtrace k exn bt

  let deep_continue_thunk k = function
  | Value v -> fun () -> Effect.Deep.continue k v
  | Exn (exn, bt) -> fun () -> Effect.Deep.discontinue_with_backtrace k exn bt

  let some_deep_continue_thunk k result = match result with
  | Value v -> fun () -> Effect.Deep.continue k (Some v)
  | Exn (exn, bt) -> fun () -> Effect.Deep.discontinue_with_backtrace k exn bt
end

module Meta = struct
  module Key = struct
    type 'a t = { id : 'a Type.Id.t; pp_value : Format.formatter -> 'a -> unit }
    let default_pp ppf _ = Format.pp_print_string ppf "<value>"
    let make ?(pp_value = default_pp) () = { id = Type.Id.make (); pp_value }
    let id k = k.id
    let pp_value k = k.pp_value
  end

  type name = string
  type binding = Binding : 'a Key.t * 'a -> binding
  type t = { name : name; bindings : binding list }

  let make ?(bindings = []) ~name () = { name; bindings }
  let name m = m.name
  let bindings m = m.bindings
  let none = make ~name:"Action.Meta.none" ()
  let choose = make ~name:"Action.choose" ()
  let is_none m = m == none
  let pp_binding ppf (Binding (k, v)) =
    Format.pp_print_space ppf (); (Key.pp_value k) ppf v

  let pp_bindings ppf bindings = List.iter (pp_binding ppf) bindings
  let pp ppf m = Format.fprintf ppf "@[%s%a@]" m.name pp_bindings m.bindings
end

module Blocked = struct
  type state = Waiting | Claimed | Synced
  type 'a t = { state : state Atomic.t; unblock : 'a Result.t -> unit }

  let make ~unblock = { state = Atomic.make Waiting; unblock }
  let with_unblock unblock b = { b with unblock }

  let none_unblock _ = assert false
  let none () = { state = Atomic.make Waiting; unblock = none_unblock }

  (* State *)

  let get_state b = Atomic.get b.state
  let set_state b st = Atomic.set b.state st
  let is_synced b = Atomic.get b.state = Synced
  let is_not_synced b = Atomic.get b.state <> Synced
  let is_not b0 b1 = b0.state != b1.state

  let exchange_waiting_to_synced b =
    let expect = Waiting and desired = Synced in
    Atomic.compare_and_exchange b.state ~expect desired

  let exchange_waiting_to_claimed b =
    let expect = Waiting and desired = Claimed in
    Atomic.compare_and_exchange b.state ~expect desired

  (* Unblock *)

  let tricky_unblock b = b.unblock
  let synced_unblock_is_ours ~candidate:result b =
    let rec loop ~backoff result b = match exchange_waiting_to_synced b with
    | Waiting -> (tricky_unblock b) result; true
    | Claimed -> loop ~backoff:(Atomic.Backoff.once backoff) result b
    | Synced -> false (* Someone else did *)
    in
    loop ~backoff:Atomic.Backoff.default result b

  let synced_unblock ~candidate blocked =
    ignore (synced_unblock_is_ours ~candidate blocked)

  (* Higher-level blocks *)

  module Value = struct
    type nonrec 'a blocked = 'a t
    type t = V : 'a Result.t * 'a blocked -> t
    let make v b = V (Result.Value v, b)
    let exchange_waiting_to_synced (V (_, b)) = exchange_waiting_to_synced b
    let exchange_waiting_to_claimed (V (_, b)) = exchange_waiting_to_claimed b
    let set_state (V (_, b)) st = set_state b st
    let is_synced (V (_, b)) = is_synced b
    let is_not_synced (V (_, b)) = is_not_synced b
    let is_not (V (_, b0)) b1 = is_not b0 b1
    let tricky_unblock (V (v, b)) = tricky_unblock b v
    let synced_unblock (V (v, b)) = synced_unblock ~candidate:v b
    let synced_unblock_is_ours (V (v, b)) =
      synced_unblock_is_ours ~candidate:v b
  end
end

(* Actions *)

type 'a poll = continue:('a Result.t -> unit -> unit) -> (unit->unit) option
type 'a block = blocked:'a Blocked.t -> unit
type 'a primitive = { meta : Meta.t; poll : 'a poll; block : 'a block }
type 'a choice = 'a primitive list
type 'a action = unit -> 'a choice
  (* XXX The thunk here is only here to support guards but we'll also use it
     for negative acks if we implement them. I think that later we should
     switch to something like the following to avoid these tower of thunks
     when these features are not used. It won't affect the API.
     | Choice of 'a choice
     | Guarded of (unit -> 'a choice) *)

let meta_of_choice =
  (* XXX We could do more sophisticated meta for choice but let's balance that
     with future implicit source locations perhaps nice things could be done. *)
  function [p] -> p.meta | _ -> Meta.choose

module Primitive = struct
  type nonrec 'a poll = 'a poll
  type nonrec 'a block = 'a block
  type 'a t = 'a primitive

  let make_bare ~meta ~poll ~block = { meta; poll; block }
  let make ~meta ~poll ~block = fun () -> [{ meta; poll; block }]
  let meta a = a.meta
  let poll_is_none ~continue = None
  let never =
    let meta = Meta.make ~name:"Action.never" () in
    { meta; poll = poll_is_none; block = fun ~blocked:_ -> () }

  let always v =
    let poll ~continue = Some (continue (Result.Value v)) in
    let block ~blocked = assert false (* someone is violating the protocol *) in
    let meta = Meta.make ~name:"Action.always" () in
    { meta; poll; block }

  (* map *)

  let map_poll f p ~continue =
    let continue = function
    | Result.Exn _ as exn -> continue exn
    | Result.Value v ->
        match f v with
        | v -> continue (Result.Value v)
        | exception exn when not (Exn.is_runtime_system exn) ->
            let bt = Printexc.get_raw_backtrace () in
            continue (Result.Exn (exn, bt))
    in
    p.poll ~continue

  let map_block f p ~blocked =
    let unblock = Blocked.tricky_unblock blocked in
    let unblock = function
    | Result.Exn _ as exn -> unblock exn
    | Result.Value v ->
        match f v with
        | v -> unblock (Result.Value v)
        | exception exn when not (Exn.is_runtime_system exn) ->
            let bt = Printexc.get_raw_backtrace () in
            unblock (Result.Exn (exn, bt))
    in
    let blocked = Blocked.with_unblock unblock blocked in
    p.block ~blocked

  let map f p = { p with poll = map_poll f p; block = map_block f p }
end

module Action = struct
  type nonrec +'a t = 'a action

  let never = fun () -> [Primitive.never]
  let always v = fun () -> [Primitive.always v]
  let choose actions = fun () -> List.concat (List.map (fun a -> a ()) actions)
  let map f action = fun () -> List.map (Primitive.map f) (action ())
  let either a0 a1 = choose [map Either.left a0; map Either.right a1]
  let guard f = fun () -> f () ()

  type 'a Effect.t +=
  | Invoke : 'a t -> 'a Effect.t
  | Invoke_poll : 'a t -> 'a option Effect.t

  let invoke action = Effect.perform (Invoke action)
  let invoke_poll action = Effect.perform (Invoke_poll action)
end

module Invocation = struct
  type 'a t = { choice : 'a choice; meta : Meta.t }

  let make ?unblock:u action = match action () with
  | choice ->
      let meta = meta_of_choice choice (* avoid the meta of [u] *) in
      let choice = match u with None -> choice | Some u -> u :: choice in
      Either.Right { choice; meta }
  | exception exn when not (Exn.is_runtime_system exn) ->
      let bt = Printexc.get_raw_backtrace () in
      Either.Left (exn, bt)

  let choice invocation = invocation.choice
  let meta invocation = invocation.meta

  let do_poll ~continue invocation =
    let poll_primitive { poll } = poll ~continue in
    let choice = choice invocation in
    try List.find_map poll_primitive choice with
    | exn when not (Exn.is_runtime_system exn) ->
        (* Defensive, poll functions are not supposed to raise. We [continue]
           the invocation with the exception rather than Exn.trap to be
           consistent with [do_block] were the rationale is more evident. *)
        let bt = Printexc.get_raw_backtrace () in
        Some (continue (Result.Exn (exn, bt)))

  let do_block ~unblock invocation =
    (* XXX We could check if the state is synced after a block to early exit.
       But we'd need to indicate in the primitive protocol that a [None] poll
       does not necessarily entail a [block] invocation. Would it be a
       problem ? *)
    let blocked = Blocked.make ~unblock in
    let block_primitive { block } = block ~blocked in
    let choice = choice invocation in
    try List.iter block_primitive choice with
    | exn when not (Exn.is_runtime_system exn) ->
        (* Defensive, block functions are not supposed to raise. We unblock
           and synchronize with the exception because if we just trap we
           likely end up blocking forever. XXX An alternative would be to Panic,
           it's a bit unclear which is best. Fail fast, fail hard ? *)
        let bt = Printexc.get_raw_backtrace () in
        Blocked.synced_unblock ~candidate:(Result.Exn (exn, bt)) blocked
end

module Unblocker = struct
  (* XXX Somehow this feels a bit specific to our scheduler. But it feels right
     to have it here and there's nothing about the scheduler here except
     for the initialization protocol. Other schedulers may want to use it aswell
     so that for example they can use the [Unix.unblocker]. The whole index
     business allows to potentially have per scheduler domain structures, but
     for example for now in the [Unix.unblocker], nothing specific is done. *)
  module Domain_local = struct
    type unblock = block:bool -> bool
    type set_block_bypass = unit -> unit
    type t = { unblock : unblock; set_block_bypass : set_block_bypass }
    let make ~unblock ~set_block_bypass () = { unblock; set_block_bypass }
    let unblock u ~block = u.unblock ~block
    let set_block_bypass u = u.set_block_bypass ()
    let none =
      let unblock ~block = assert false in
      let set_block_bypass () = assert false in
      make ~unblock ~set_block_bypass ()

    let nothing () =
      let block_bypass = ref false and m = Mutex.create () in
      let wakeup = Condition.create () in
      let unblock ~block =
        if not block then false else
        Mutex.protect m @@ fun () ->
        while not !block_bypass do Condition.wait wakeup m done;
        block_bypass := false; false
      in
      let set_block_bypass () = Mutex.protect m @@ fun () ->
        block_bypass := true; Condition.broadcast (* in case *) wakeup
      in
      make ~unblock ~set_block_bypass ()
  end
  type index = int
  type init = domain_count:int -> unit
  type domain_local = index -> Domain_local.t
  type domain_install = Domain_local.t -> unit
  type domain_uninstall = Domain_local.t -> unit
  type deinit = unit -> unit
  type t =
    { init : init; domain_local : domain_local; domain_install : domain_install;
      domain_uninstall : domain_uninstall; deinit : deinit }

  let make ~init ~domain_local ~domain_install ~domain_uninstall ~deinit () =
    { init; domain_local; domain_install; domain_uninstall; deinit }

  let init u = u.init
  let domain_local u = u.domain_local
  let domain_install u = u.domain_install
  let domain_uninstall u = u.domain_uninstall
  let deinit u = u.deinit
  let nothing () =
    (* We use the same local for all domains because the scheduler guarantees
       there is a single one that blocks. Lots of nops here. *)
    let local = Domain_local.nothing () in
    let init ~domain_count:_ = () and deinit () = () in
    let domain_local _i = local in
    let domain_install _l = () and domain_uninstall _l = () in
    make ~init ~domain_local ~domain_install ~domain_uninstall ~deinit ()
end

type unblocker = Unblocker.t

include Action

module Private = struct
  module Action = struct
    include Action
    type nonrec 'a primitive = 'a primitive
    type nonrec unblocker = unblocker
    module Result = Result
    module Meta = Meta
    module Invocation = Invocation
    module Blocked = Blocked
    module Primitive = Primitive
    module Unblocker = Unblocker

    let do_invoke_poll ?unblock action k =
      match Invocation.make ?unblock action with
      | Either.Left (exn, bt) -> Effect.Deep.discontinue_with_backtrace k exn bt
      | Either.Right invocation ->
          let continue = Result.some_deep_continue_thunk k in
          match Invocation.do_poll invocation ~continue with
          | Some continue -> continue ()
          | None -> Effect.Deep.continue k None
  end
end
