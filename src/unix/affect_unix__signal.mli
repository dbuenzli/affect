(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Waiting on signals.

    See also {!Affect_unix.Unix.Signal}. *)

open Affect
open Action.Private

(** Waiters unblocker. *)
module Unblocker : sig
  type t
  (** The type for unblocking action invocations waiting on signals. *)

  val make : unit -> t
  (** [make ()] is a new signal unblocker. *)

  val is_empty : t -> bool
  (** [is_empty u] is [true] iff there are no blocked action invocations in
      [u]. *)

  val add_wait :
    signal:Sys.signal -> blocked:Action.Blocked.Value.t -> t -> unit
  (** [add_wait ~signal ~blocked] waits until the next occurence
      of [signal] (if its handler is set to {!Waiters} at that point)
      and tries unblock [blocked]. *)

  val unblock : t -> bool
  (** [unblock u] unblocks the waiters and returns [true] if we did unblock
      at least one. *)

  (** {1:domain_local Domain local unblocker} *)

  val get_domain_local : unit -> t
  (** [get_domain_local ()] gets the domain local signal unblocker (which is
      usually shared among the domains of a scheduler). This raises
      [Invalid_argument] if no signal unblocker was set with {!set} or if it
      was {!clear}ed. *)

  val set_domain_local : t -> unit
  (** [set_domain_local t] sets the domain local signal unblocker on this
      domain to [t]. *)

  val clear_domain_local : unit -> unit
  (** [clear_domain_local ()] clears the domain local signal unblocker on this
      domain. *)

  (** {1:global Global register of domain local unblockers} *)

  val register : t -> unit
  (** [register b] registers [b] for use by signal handler. This is needed
      due to the global nature of signal handling. *)

  val unregister : t -> unit
  (** [unregisters b] for use by signal handler. *)
end

(** {1:syscall_unblocking Syscall unblocking} *)

val syscall_block_bypass_fd : Unix.file_descr
(** [syscall_block_bypass_fd] should be added to the read set of a syscall
    watching for fd readiness. It allows to unblock the call reliably
    regardless on how signal delivery is defined in the system.
    You may still get sporadic [EINTR]s though. After unblocking
    it's a good idea to call {!clear_syscall_block_bypass} regardless on
    how your syscall unblocks. *)

val clear_syscall_block_bypass : unit -> unit
(** [clear_syscall_block_bypass ()] drains any data that may be available in
    {!syscall_unblock_fd}. This is safe to call regardless of whether
    there is data. *)

(** {1:api API}

    See {!Affect_unix__unix.Signal} *)

val wait : Sys.signal -> unit
val wait_any : Sys.signal list -> Sys.signal
val wait' : Sys.signal -> 'a -> 'a Action.t

type handler = Default | Ignore | Fun of (Sys.signal -> unit) | Waiters
val set : Sys.signal -> handler -> unit
val set_and_restore : Sys.signal -> handler -> (unit -> 'a) -> 'a

val pp : Format.formatter -> Sys.signal -> unit
