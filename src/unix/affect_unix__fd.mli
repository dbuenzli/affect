(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** File descriptors tooling. *)

open Affect__base
open Affect.Action.Private

(** File descriptors. *)
module Fd : sig
  module type UNBLOCKER = sig
    type t
    (** The type for unblocking action invocations waiting on fd readiness. *)

    val make : unit -> t
    (** [make ()] is a new file descriptor unblocker. {!destroy}
        must be called on the result when you are done. *)

    val dispose : t -> unit
    (** [dispose u] destroys [u]. *)

    val is_empty : t -> bool
    (** [is_empty u] is [true] iff there are no blocked action invocations in
        [u]. *)

    val add_wait_readable :
      t -> Unix.file_descr -> blocked:Action.Blocked.Value.t -> unit
    (** [add_wait_readable u fd ~blocked] waits for [fd] to become
        readable and tries to unblock [blocked]. *)

    val add_wait_writable :
      t -> Unix.file_descr -> blocked:Action.Blocked.Value.t -> unit
    (** [add_wait_writable u fd ~blocked] waits for [fd] to become
        writable  and tries to unblock [blocked]. *)

    val unblock : t ->
      timeout_ns:Affect_unix__timeline.mtime_span_ns option -> bool
    (** [unblock u ~timeout_ns] must try to unblock file descriptors
        and return [true] if did unblock at least one. [timeout_ns]
        is such that:
        {ul
        {- [None], means block forever.}
        {- [Some 0L], means don't block.}
        {- [Some dur], means returns at most after [dur].}
        You should handle [EINTR] by simply exiting with [false].
        You must also handle the {!Affect_unix__signal.syscall_unblock_fd}
        and {!Affect_unix__signal.syscall_clear_unblock} dance so that
        signals are handled correctly. Besides you must implement
        the {!set_block_bypass} scheme (e.g. with {!Flagfd}). *)

    val set_block_bypass : t -> unit
    (** [set_block_bypass t] indicates that the current or next
        call to {!unblock} should not block. *)

    (** {1:domain_local Domain local unblocker} *)

    val get_domain_local : unit -> t
    (** [get_domain_local ()] gets the domain local fd unblocker. This raises
        [Invalid_argument] if no fd unblocker was set with {!set} or if it
        was {!clear}ed. *)

    val set_domain_local : t -> unit
    (** [set_domain_local t] sets the domain local fd unblocker on this
        domain to [t]. *)

    val clear_domain_local : unit -> unit
    (** [clear_domain_local ()] clears the domain local fd unblocker on this
        domain. *)
  end

  (** {1:maps Maps} *)

  (** Map of file *)
  module Map : Map.S
  module Synchronized_map : Synchronized_map.T
    with type 'a map = 'a Map.t
     and type key = Unix.file_descr
end

(** Just a bit to unblock system calls blocked on fds forever *)
module Flagfd : sig
  type t
  val make : unit -> t
  val set : t -> unit
  val clear : t -> unit
  val fd : t -> Unix.file_descr
  val dispose : t -> unit
end
