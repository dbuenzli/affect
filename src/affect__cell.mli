(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Synchronized cells.

    @canonical Affect.Cell *)

(** Drop cells.

    A drop cell is a cell on which functions can exchange values:
    {ul
    {- A {!Drop.take} blocks until the cell is full and empties the cell.}
    {- A {!Drop.put} blocks until the cell is empty and fills the cell.}} *)
module Drop : sig

  (** {1:cells Cells} *)

  type 'a t
  (** The type for drop cells with value of type ['a]. *)

  val make : unit -> 'a t
  (** [make ()] is a new empty drop cell. *)

  val take : 'a t -> 'a
  (** [take cell] blocks until the cell is full with [v], empties the cell
      and continues with [v]. If there are multiple blocked [take] and
      a single {!put} occurs, only the earliest [take] unblocks. *)

  val put : 'a t -> 'a -> unit
  (** [put cell v] blocks until the cell is empty and fills it with [v].
      If there are multiple blocked [put] on the same cell and a single
      {!take} occurs, only the earliest [put] unblocks. *)

  (** {1:pred Predicate} *)

  val is_empty : 'a t -> bool
  (** [is_empty cell] is [true] if and only if [cell] is empty. *)

  (** {1:actions Actions} *)

  val take' : 'a t -> 'a Affect__action.t
  (** [take' cell] is the action for {!take}. An action invocation
      is enabled when [cell] is filled with a value [v]. The invocation
      synchronizes with [v] if it manages to empty the [cell] by taking [v]. *)

  val put' : 'a t -> 'a -> 'tag -> 'tag Affect__action.t
  (** [put' cell v tag] is the action for {!put}. An action invocation is
      enabled when [cell] is empty. The invocation synchronizes with
      [tag] if it manages to put [v] in [cell]. *)
end

(** Lazy immutable cells.

    A lazy immutable cell is an immutable cell whose content
    is computed exactly once:
    {ul
    {- A {!Lazy.force} blocks until the cell's value is computed.}} *)
module Lazy : sig

  (** {1:cells Cells} *)

  type 'a t
  (** The type for lazy immutable cells. *)

  val make : (unit -> 'a) -> 'a t
  (** [make f] is a new lazy cell computed with [f ()] by the first function
      that calls {!force}[ l]. *)

  val from_val : 'a -> 'a t
  (** [from_val v] is a lazy cell already forced to [v]. *)

  val from_exn : exn -> Printexc.raw_backtrace -> 'a t
  (** [from_exn exn bt] is a lazy cell already forced to the exception
      [exn] with backtrace [bt]. *)

  val force : 'a t -> 'a
  (** [force cell] blocks until [cell]'s value (or exception) is computed
      and continues with it. The first caller to [force] on [cell] doesn't
      block, it executes the thunk and continues with the result. *)

  (** {1:predicates Predicates} *)

  val is_forced : 'a t -> bool
  (** [is_forced cell] is [true] if and only if [cell] has been forced. *)

  val is_val : 'a t -> bool
  (** [is_val cell] is [true] if and only if [cell] has been forced and did
      not raise an exception. *)

  (** {1:actions Actions} *)

  val force' : 'a t -> 'a Affect__action.t
  (** [force' cell] is the action for {!force}. An action invocation
      is permanently enabled and synchronizes with the [cell]'s value
      (or exception) once it has been computed by the first action
      invocation. *)
end

(** Set-once immutable cells.

    A set-once immutable cell is a cell that can be set exactly once:
    {ul
    {- A {!Once.try_set} sets the value of the cell if still unset.}
    {- A {!Once.get} blocks until the value of the cell is set.}} *)
module Once : sig

  (** {1:cells Cells} *)

  type 'a t
  (** The type for set-once immutable cells. *)

  val make : unit -> 'a t
  (** [make ()] is a new set-once immutable cell. *)

  val from_val : 'a -> 'a t
  (** [from_val v] is a cell already set to [v]. *)

  val try_set : 'a t -> 'a -> unit
  (** [try_set cell v] attempts to set [cell] to [v] if not already set. *)

  val try_set_is_ours : 'a t -> 'a -> bool
  (** [try_set_is_ours cell v] attempts to set [cell] to [v] and returns:
      {ul
      {- [true] if the cell was unset and set to [v] by the call.}
      {- [false] if the cell was already set, in which case nothing happened.}}
  *)

  val get : 'a t -> 'a
  (** [get cell] blocks until [cell]'s value is set to [v] and continues with
      [v]. *)

  (** {1:preds Predicates} *)

  val is_set : 'a t -> bool
  (** [is_set cell] is [true] if and only if [cell] is set. *)

  (** {1:actions Actions} *)

  val get' : 'a t -> 'a Affect__action.t
  (** [get' cell] is the action for {!get}. An action invocation is permanently
      enabled and synchronizes with [v] once the [cell] has been set to [v]. *)
end
