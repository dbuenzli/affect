(*---------------------------------------------------------------------------
   Copyright (c) 2024 The affect programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

(* This implements the following fiber structure

  x = tries is None

  t----x----x------x------x----x--------- …
  |
--seqs----------------------------------- …
  |
  |           t---x--x---x----x------x--- …
  |  delay_s  |
  ts----------seqs----------------------- …
              |
              |            t-------x----- …
              |  delay_s   |
              ts--------- seqs----------- …
                           |
                           …  *)

(* TODO check in ts depends on semantics of sleep_s
   TODO is Fiber.pick_either really enlighting ? Its more to remember.
   We could also have only  Fiber.either and a cancel_and_await. *)


let queue l = (* Need to make that Queue thread safe *)
  let q = Queue.of_seq (List.to_seq l) in
  let m = Mutex.create () in
  let is_empty () = Mutex.protect m @@ fun () -> Queue.is_empty q in
  let next () = Mutex.protect m @@ fun () -> Queue.take_opt q in
  is_empty, next

let happy_eyeballs :
    delay_s:float -> tries:(unit -> 'a option) list -> discard:('a -> unit) ->
    'a option
=
fun ~delay_s ~tries ~discard ->
  let is_empty, next = queue tries in
  let rec seq () =
    if Fiber.self_is_cancelled () then None else match next () with
    | None -> None
    | Some trial -> match trial () with None -> seq () | Some _ as v -> v
  in
  let rec seqs () =
    if is_empty () || Fiber.self_is_cancelled () then None else
    let t = Fiber.async seq in
    let ts = Fiber.async @@ fun () -> match Funix.sleep_s delay_s with
      | exception Fiber.Cancelled -> None
      | () -> seqs ()
    in
    match Fiber.pick_either t ts with
    | Either.Left t -> Option.iter discard (Fiber.await ts); t
    | Either.Right ts -> Option.iter discard (Fiber.await t); ts
  in
  seqs ()
