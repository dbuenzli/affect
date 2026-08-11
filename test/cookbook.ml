(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Affect
open Affect_unix

(* Basics *)

let array_parallel_map_inplace : ('a -> unit) -> 'a array -> unit =
fun f a ->
  Fun.Async.get @@ Fun.Async.call @@ fun () ->
  let size = Array.length a in
  let worker_count = Fun.Async.parallel_worker_count () in
  let worker_count, range = Fun.Async.divide_work ~size ~worker_count in
  for w = 0 to worker_count - 1 do
    Fun.Async.call_trap_exn @@ fun () ->
    let first, last = range w in
    for i = first to last do a.(i) <- f a.(i) done
  done

(* Effects *)

type _ Effect.t += Incr : unit Effect.t
let incr () = Effect.perform Incr
let make_counter () =
  let c = Atomic.make 0 in
  let handle f = match f () with
  | v -> v
  | effect Incr, k -> Atomic.incr c; Effect.Deep.continue k ()
  in
  c, Fun.Async.Call_handler.{handle}

let () =
  let c0, handle_c0 = make_counter () in
  let c1, handle_c1 = make_counter () in
  Fun.Async.main ~handler:handle_c0 @@ fun () ->
  incr ();
  let f = Fun.Async.call @@ fun () ->
    incr ();
    Fun.Async.call_trap_exn ~handler:handle_c1 (fun () -> incr ());
    incr ();
  in
  incr ();
  Fun.Async.get f;
  assert (Atomic.get c0 = 4 && Atomic.get c1 = 1);
  ()

(* Why is my function not returning? *)

let f () =
  Fun.Async.call @@ fun () ->
  let p = Port.make () in
  ignore @@ Fun.Async.call (fun () -> Port.offer p 34);
  ignore @@ Fun.Async.call (fun () -> Port.offer p (Port.take p));
  "Please return"

let main () = Fun.Async.main @@ fun () ->
  Fun.Async.get (f ()) (* blocks forever *)

(* Tracing *)

let () = Fun.Async.Trace.(set_reporter (only_fun stderr_reporter))

(* Blueprint for defining actions *)

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
