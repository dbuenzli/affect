(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect__base
open Affect.Action.Private

(* File descriptors *)

module Fd = struct
    module type UNBLOCKER = sig
      type t
      val make : unit -> t
      val dispose : t -> unit
      val is_empty : t -> bool

      val add_wait_readable :
        t -> Unix.file_descr -> blocked:Action.Blocked.Value.t -> unit

      val add_wait_writable :
        t -> Unix.file_descr -> blocked:Action.Blocked.Value.t -> unit

      val unblock : t ->
        timeout_ns:Affect_unix__timeline.mtime_span_ns option -> bool

      val set_block_bypass : t -> unit
      val get_domain_local : unit -> t
      val set_domain_local : t -> unit
      val clear_domain_local : unit -> unit
  end

  module T = struct type t = Unix.file_descr let compare = Repr.compare end
  module Map = Map.Make (T)
  module Synchronized_map = Synchronized_map.Make (Map)
end

module Flagfd = struct
  (* Thanks to MisterDA this should work cross-platform. But we
     could plug platform specific stuff like [eventfd] here. *)

  type t =
    { set : Unix.file_descr; (* send *)
      clear : Unix.file_descr; (* recv *)
      clear_buf : Bytes.t }

  let clear_buf_size = 64
  let make () =
    let set, clear = Unix.socketpair ~cloexec:true PF_UNIX SOCK_STREAM 0 in
    let clear_buf = Bytes.make clear_buf_size '\x00' in
    Unix.set_nonblock set; Unix.set_nonblock clear;
    { set; clear; clear_buf }

  let set flag = try ignore (Unix.write_substring flag.set "1" 0 1) with
  | Unix.Unix_error ((EAGAIN|EWOULDBLOCK), _, _) -> ()

  let rec clear flag =
    try
      let len = Bytes.length flag.clear_buf in
      if Unix.read flag.clear flag.clear_buf 0 len = len
      then clear flag else ()
    with
    | Unix.Unix_error ((EAGAIN|EWOULDBLOCK), _, _) -> ()

  let fd flag = flag.clear
  let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()
  let dispose flag = close_noerr flag.set; close_noerr flag.clear
end
