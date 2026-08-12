(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Semaphores.

    A semaphore with capacity [n] throttles access to a ressource with
    limited capacity [n].

    {ul
    {- An {!acquire} blocks until the semaphore's value can be
       decremented to a non-negative integer.}
    {- A {!release} increments the value of the semaphore. It doesn't
       block but it is a programming error to increment it over [n].}}

    @canonical Affect.Semaphore *)

(** {1:semaphores Semaphores} *)

type t
(** The type for semaphores. *)

val make : int -> t
(** [make n] is a semaphore with a capacity of [n].

    @raise Invalid_argument if [n < 0]. *)

val acquire : t -> unit
(** [acquire s] blocks until [s] can be decremented to a non-negative
    integer. *)

val release : t -> unit
(** [release s] increments the value of [s]. If there are multiple
    blocked [acquire] on [s], only the earliest [acquire] unblocks.

    @raises Invalid_argument if value of [s] exceeds the capacity
    after the increment. {b XXX.} The stdlib uses [Sys_error],
    check why. *)

val with_acquired : t -> (unit -> 'a) -> 'a
(** [with_acquired s f] acquires [s] executes [f ()]
    and releases [s] when [f] returns a value or an exception. *)

(** {1:properties Properties} *)

val get_value : t -> int
(** [get_value s] is the current value of semaphore [s]. *)

val capacity : t -> int
(** [capacity s] is the capacity of [s]. *)

(** {1:actions Actions} *)

val acquire' : t -> 'tag -> 'tag Affect__action.t
(** [acquire' s tag] is the action for {!acquire}. An action invocation
    is enabled when [s] is non-zero. The invocation synchronizes with
    [tag] if it manages to decrement [s] to a non-negative value. *)
