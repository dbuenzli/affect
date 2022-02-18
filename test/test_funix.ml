(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)


let log fmt = Printf.printf (fmt ^^ "\n%!")

let flip = let () = Random.self_init () in Random.bool
let now_s () = Unix.gettimeofday ()

let test_sleeps () =
  log "Testing simple sleep.";
  let main () =
    let rec loop start max n =
      if n > max then log " Took %02fs" (now_s () -. start) else
      let dur_s = 0.25 in
      log " Sleeping for %.02fs" dur_s;
      Funix.sleep_s dur_s;
      loop start max (n + 1)
    in
    loop (now_s ()) 3 0
  in
  ignore (Fiber.run ~unblock:Funix.unblock main)

let test_either_sleep () =
  log "Testing either shortest sleep.";
  let left_ret = ref false in
  let right_abort = ref false in
  let main () =
    let left = Fiber.spawn @@ fun () ->
      Funix.sleep_s 0.5;
      left_ret := true
    in
    let right = Fiber.spawn @@ fun () ->
      (try Funix.sleep_s 1.0 with
      | Fiber.Abort -> right_abort := true; raise Fiber.Abort);
      assert false
    in
    match Fiber.either left right with
    | Some Either.Left () -> ()
    | Some Either.Right () -> assert false
    | None -> assert false
  in
  ignore (Fiber.run ~unblock:Funix.unblock main);
  assert (!left_ret);
  assert (!right_abort)

let main () =
  test_sleeps ();
  test_either_sleep ();
  log "Success!"

let () = if !Sys.interactive then () else main ()

(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
