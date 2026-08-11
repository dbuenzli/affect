(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

open Affect

module M : sig
  val op : unit -> 'a
  (** [op ()] blocks until <condition>, <effect> and continues with <value>. *)

  val op' : 'a Action.t
  (** [op'] is the action for {!op}. An action invocation is enabled when
      <condition> and it synchronizes with <value> [if <condition>]. *)
end = struct
  open Action.Private
  let op_poll ~continue = failwith "TODO"
  let op_block ~blocked = failwith "TODO"
  let op_meta = Action.Meta.make ~name:"M.op" ()
  let op' = Action.Primitive.make ~meta:op_meta ~poll:op_poll ~block:op_block
  let op () = Action.invoke op'
end
