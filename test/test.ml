(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

type log = (unit, out_channel, unit, unit, unit, unit) format6 -> unit
let log fmt = Printf.printf (fmt ^^ "\n%!")
let log_branch id fmt =
  let id = String.concat "." (List.map string_of_int id) in
  Printf.printf (" [%s]: " ^^ fmt ^^ "\n%!") id

let flip = let () = Random.self_init () in Random.bool

let test_simple_spawns () =
  log "Testing simple spawn joining.";
  let yield (log : log) = log "Yielding…"; Fiber.yield (); log "Comming back!"in
  let branch n v = Fiber.spawn @@ fun () ->
    let log = log_branch [n] in
    log "Starting"; if flip () then yield log; v
  in
  let main () =
    let branch n v = if flip () then yield (log_branch []); branch n v in
    let msgs = ["Hey"; "ho,"; "let's go!"] in
    let bs = List.mapi branch msgs in
    let rs = List.filter_map Fiber.join bs in
    log " result: %s" (String.concat " " rs);
    assert (msgs = rs)
  in
  ignore (Fiber.run main)

let test_abort_tree () =
  log "Testing subtree abort";
  let max = 2 in
  let rec spawn depth rpath () =
    let log = log_branch (List.rev rpath) in
    try
      log "Starting…";
      (if depth < max
       then (ignore (Fiber.spawn (spawn (depth + 1) (1 :: rpath)));
             ignore (Fiber.spawn (spawn (depth + 1) (2 :: rpath)))));
      for i = 0 to max - depth do Fiber.yield () done;
      if flip () && flip () then (log "Self aborting!"; Fiber.self_abort ());
      for i = 0 to max - depth do Fiber.yield (); done;
      log "Finished"
    with Fiber.Abort -> log "Aborting"; raise Fiber.Abort
  in
  let main () = spawn 0 [] () in
  ignore (Fiber.run main)

let test_abort () =
  log "Testing potential abort";
  let did_close = ref false in
  let sub () =
    let finally () = did_close := true in
    Fun.protect ~finally @@ fun () ->
    Fiber.yield ();
    log " Fiber finished";
  in
  let main () =
    let f = Fiber.spawn sub in
    Fiber.yield ();
    if flip () then (log " Aborting fiber!"; Fiber.abort f)
  in
  ignore (Fiber.run main);
  assert (!did_close)

let test_lone () =
  log "Testing lone fiber run.";
  let did_it = ref false in
  let main () =
    Fiber.yield ();
    Fiber.yield ();
    did_it := true
  in
  ignore (Fiber.run main);
  assert (!did_it)

let test_main_waits () =
  log "Testing main waits.";
  let did_it = ref false in
  let sub () = Fiber.yield (); did_it := true in
  let main () = ignore (Fiber.spawn sub) in
  ignore (Fiber.run main);
  assert (!did_it)

let test_main_abort () =
  log "Testing main aborts.";
  let sub_did_abort = ref false in
  let sub () = try Fiber.yield () with
  | Fiber.Abort -> sub_did_abort := true; raise Fiber.Abort
  in
  let main () =
    ignore (Fiber.spawn sub);
    Fiber.yield (); (* If we don't, sub doesn't even execute because of abort*)
    Fiber.self_abort ()
  in
  ignore (Fiber.run main);
  assert (!sub_did_abort)

let test_blocking () =
  log "Testing block";
  let unblock, next =
    let i = ref (-2) in
    let blocked = ref [] in
    let neq e = Fun.negate (Fiber.E.equal e) in
    let rem_blocked e = blocked := List.filter (neq e) !blocked in
    let pop_blocked () = match !blocked with
    | [] -> None | e :: es -> rem_blocked e; Some e
    in
    let block e =
      log " Blocking %d" (Fiber.E.id e);
      blocked := e :: !blocked
    in
    let abort e = log " Aborting"; rem_blocked e in
    let retv e = rem_blocked e; incr i; !i in
    let unblock ~poll = match poll with
    | false -> pop_blocked ()
    | true ->
        if !i = (-2) then (* for testing abort *) (incr i; None) else
        if flip () then pop_blocked () else None
    in
    let next () = Fiber.block ~block ~abort ~retv in
    unblock, next
  in
  let aborted = ref false in
  let sub_abort () =
    log " Blocking (will abort)";
    let i = try next () with
    | Fiber.Abort -> log "Aborted"; aborted := true; raise Fiber.Abort
    in
    aborted := false;
    log " This should not be printed: %d next" i;
  in
  let sub () =
    Fiber.yield ();
    log " Blocking";
    let i = next () in
    log " Unblocked %d" i
  in
  let main () =
    let f = Fiber.spawn sub_abort in
    Fiber.yield ();
    Fiber.abort f;
    ignore (Fiber.spawn sub);
    Fiber.yield ();
    ignore (Fiber.spawn sub)
  in
  ignore (Fiber.run ~unblock main);
  assert (!aborted)

let main () =
  test_simple_spawns ();
  test_abort_tree ();
  test_abort ();
  test_lone ();
  test_main_waits ();
  test_main_abort ();
  test_blocking ();
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
