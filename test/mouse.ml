(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

(* Compile and run with:

   ocamlfind ocamlopt -package tsdl,affect -linkpkg -thread -o mouse mouse.ml
   ./mouse

   mouse runs two fiber. One asks for one click, the other for two. *)

let strf = Format.asprintf

open Tsdl

(** Fiber friendly SDL functions *)
module Fsdl : sig

  val with_window : (Sdl.window -> 'a) -> ('a, string) result
  (** [with_window work] runs [work] with a window setup. *)

  (** {1:unblock Fiber unblocking} *)

  val unblock : Fiber.unblock
  (** [unblock] is the function to unblock fibers blocked by the function
      of this module. You must use this function with {!Fiber.val-run}. *)

  (** {1:wait Events} *)

  val mouse_button_up : unit -> (int * int)
  (** [mouse_button_up ()] are the coordinates of the next mouse button
      up event. *)
end = struct

  let with_window work = match Sdl.init Sdl.Init.video with
  | Error (`Msg e) -> Error (strf "Init error: %s" e)
  | Ok () ->
      let flags = Sdl.Window.(shown + mouse_focus + resizable) in
      match Sdl.create_window ~w:640 ~h:480 "Mouse" flags with
      | Error (`Msg e) -> Error (strf "Create window error: %s" e)
      | Ok w ->
          Sdl.start_text_input ();
          let finally () = Sdl.destroy_window w; Sdl.quit () in
          Ok (Fun.protect ~finally (fun () -> work w))

  (* Per domain blocking data structure.

     The domain local storage is likely useless here. I bet you can't access
     SDL except from the main domain.

     This could easily be generalized to block on any event, we just
     use mouse up for demonstration purposes. *)

  module Fmap = Map.Make (Fiber.E)
  type blocked =
    { mutable mouse_button_up : (int * int) ref Fmap.t;
      ready : Fiber.E.t Queue.t; }

  let blocked =
    let blocked_make () =
      { mouse_button_up = Fmap.empty; ready = Queue.create () }
    in
    let blocked = Domain.DLS.new_key blocked_make in
    fun () -> Domain.DLS.get blocked

  let mouse_button_up () =
    let b = blocked () in
    let cell = ref (0, 0) in
    let block f = b.mouse_button_up <- Fmap.add f cell b.mouse_button_up in
    let abort f = b.mouse_button_up <- Fmap.remove f b.mouse_button_up in
    let retv _ = !cell in
    Fiber.block ~block ~abort ~retv

  let e = Sdl.Event.create ()
  let wait ~poll b =
    let handle_event b e = match Sdl.Event.(enum (get e typ)) with
    | `Mouse_button_up ->
        let loc = Sdl.Event.(get e mouse_button_x, get e mouse_button_y) in
        let unblock f cell = cell := loc; Queue.add f b.ready in
        Fmap.iter unblock b.mouse_button_up; b.mouse_button_up <- Fmap.empty;
        Queue.take_opt b.ready
    | _ -> None
    in
    match poll with
    | true -> if Sdl.poll_event (Some e) then handle_event b e else None
    | false ->
        match Sdl.wait_event (Some e) with
        | Ok () -> handle_event b e
        | Error (`Msg e) -> Sdl.log "Could not wait event: %s" e; None

  let unblock ~poll =
    let b = blocked () in
    match Queue.take_opt b.ready with
    | Some _ as f -> f
    | None -> wait ~poll b
end

let pp_point ppf (x, y) = Format.fprintf ppf "(%d,%d)" x y

let of_mice_and_men () =
  let wait_click () =
    let loc = Fsdl.mouse_button_up () in
    Sdl.log "Click: %a\n" pp_point loc
  in
  let twice () = wait_click (); Sdl.log "One more please!"; wait_click () in
  Sdl.log "Please have a click!";
  ignore (Fiber.spawn wait_click);
  ignore (Fiber.spawn twice);
  ()

let main () =
  let run _w = Fiber.run ~unblock:Fsdl.unblock of_mice_and_men in
  match Fsdl.with_window run with
  | Error e -> Sdl.log "%s" e; exit 1
  | Ok None -> Sdl.log "Of mice and men fiber aborted"; exit 1
  | Ok Some () -> exit 0

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
