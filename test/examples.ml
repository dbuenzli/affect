(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Affect
open Affect_unix

(* Action module *)

let fancy_wait_readable :
  Unix.file_descr -> [ `Readable | `Timeout | `User1_interrupt ] Action.t
=
fun fd ->
  let readable = Unix.wait_readable fd `Readable in
  let timeout = Mtime.wait_for' Mtime.Span.(1*min + 30*s) `Timeout in
  let signal = Unix.Signal.wait' Sys.sigusr1 `User1_interrupt in
  Action.choose [readable; timeout; signal]

let timed_wait_readable : Unix.file_descr -> Mtime.Span.t Action.t =
fun fd ->
  let timed () =
    let counter = Mtime.counter () in
    let readable = Unix.wait_readable fd counter in
    Action.map Mtime.count readable
  in
  Action.guard timed

let wait_readable_duration : Unix.file_descr -> Mtime.Span.t Action.t =
fun fd ->
  let setup () =
    let counter = Mtime.counter () in
    let readable = Unix.wait_readable fd counter in
    Action.map Mtime.count readable
  in
  Action.guard setup

let no_speculation_get : 'a Fun.Async.t -> 'a option Action.t =
fun f ->
  let wait_cancelled = Fun.Async.wait_cancelled f None in
  let get = Action.map Option.some (Fun.Async.get' f) in
  Action.choose [wait_cancelled; get]

(* Port module *)

let f () =
  let p = Port.make () in
  let v = Fun.Async.call @@ fun () -> Port.take p in
  let _call = Fun.Async.call @@ fun () -> Port.offer p "hey!" in
  assert (Fun.Async.get v = "hey!")

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

let f () =
  let c = Fun.Async.call @@ fun () -> 3.14 in
  let p = Port.make () in
  let _c = Fun.Async.call @@ fun () -> Port.offer p (Fun.Async.get' c) in
  let v = Fun.Async.call @@ fun () -> Action.invoke (Port.take p) in
  assert (Fun.Async.get v = 3.14)
