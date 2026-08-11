(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Synchronous one-to-one rendezvous.

    A port provides a handle on which functions can exchange values with
    a one-to-one synchronous rendezvous:

    {ul
    {- A {!Port.offer} blocks until there is matching {!Port.take} call.}
    {- A {!Port.take} blocks until there is matching {!Port.offer} call.}}

    See {{!examples}examples}.

    @canonical Affect.Port.t *)

(** {1:ports Ports} *)

type 'a t
(** The type for ports with values of type ['a]. *)

val make : unit -> 'a t
(** [make ()] is a new port. *)

val offer : 'a t -> 'a -> unit
(** [offer p v] blocks until there is matching {!take}[ p] call to which it
    transfers the value [v]. If there are multiple [offer] on [p] and a
    single {!take} only the earliest [offer] unblocks. *)

val take : 'a t -> 'a
(** [take p] blocks until there is a matching {!offer}[ p v] call and continues
    with [v]. If there are multiple [take] on [p] and a single {!offer}
    only the earliest [take] unblocks. *)

(** {1:actions Actions} *)

val offer' : 'a t -> 'a -> 'tag -> 'tag  Affect__action.t
(** [offer' p v tag] is the action for {!offer}. An action invocation is
    enabled if there is an enabled {!take}[ p] invocation. The
    invocation synchronizes with [tag] if it manages to simultaneously
    synchronize an enabled {!take}[ p] invocation with its offer. *)

val take' : 'a t -> 'a Affect__action.t
(** [take' p] is the action for {!take}. An action invocation is enabled
    if there is an enabled {!offer}[ p v] invocation. The invocation
    synchronizes with [v] if it manages to simultaneously synchronize an
    enabled {!offer}[ p v] invocation. *)

(** {1:examples Examples}

    This shows how two functions can be made to synchronize via a port.
{[
let f () =
  let p = Port.make () in
  let v = Fun.Async.call @@ fun () -> Port.take p in
  let _call = Fun.Async.call @@ fun () -> Port.offer p "hey!" in
  assert (Fun.Async.get v = "hey!")
]}

    The following example is from the {{!Action.basics}action basics}
    section, it defines an action derived from port actions that allows two
    functions to agree on a value both may propose. It also
    shows that {!Affect.Action.choose} is really a choice: only one action
    returns, so in [propose']the offer and take on the same port in the same
    choice cannot match together.
{[
let propose' p v =
  let ours = Port.offer' p v v in
  let theirs = Port.take' p in
  Action.choose [ours; theirs]

let propose p v = Action.invoke (propose' p v)

let f () =
  let p = Port.make () in
  let v0 = Fun.Async.call @@ fun () -> propose p "hey!" in
  let v1 = Fun.Async.call @@ fun () -> propose p "ho!" in
  let v0, v1 = Fun.Async.get v0, Fun.Async.get v1 in
  assert (v0 = v1 && (v0 = "hey!" || v0 = "ho!"))
]}

In this example we show actions are first-class values, they can be
transferred over ports.
{[
let f () =
  let c = Fun.Async.call @@ fun () -> 3.14 in
  let p = Port.make () in
  let _c = Fun.Async.call @@ fun () -> Port.offer p (Fun.Async.get' c) in
  let v = Fun.Async.call @@ fun () -> Action.invoke (Port.take p) in
  assert (Fun.Async.get v = 3.14)
]}
*)
