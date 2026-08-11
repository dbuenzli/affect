(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: CC0-1.0
  ---------------------------------------------------------------------------*)

open Affect

module Image = struct
  type t = { w : int; h : int; luminance : Bytes.t }
  let max_l = 255
  let w i = i.w
  let h i = i.h
  let make ~w ~h ~l:c = { w; h; luminance = Bytes.make (w * h) (Char.chr c) }
  let set i ~x ~y ~l = Bytes.set_uint8 i.luminance (y * i.w + x) l
  let get i ~x ~y = Bytes.get_uint8 i.luminance (y * i.w + x)
  let write_pgm file i =
    try
      Out_channel.with_open_bin file @@ fun oc ->
      output_string oc (Printf.sprintf "P5 %d %d %d\n" i.w i.h max_l);
      output_bytes oc i.luminance;
      Ok ()
    with
    | Sys_error e -> Error e
end

let pixel_to_complex image ~x ~y ~top_left:(tx, ty) ~size:(w, h) =
  Complex.{ re = tx +. (float x) *. (w /. float (Image.w image));
            im = ty -. (float y) *. (h /. float (Image.h image)); }

let escape_time ~limit c =
  let rec loop ~limit i c z =
    if i >= limit then None else
    if Complex.norm2 z > 4. then Some i else
    loop ~limit (i + 1) c Complex.(add (mul z z) c)
  in
  loop ~limit 0 c Complex.zero

let render image ~center:(cx, cy) ~size:(w, h as size) ~max_iteration =
  Fun.Async.call @@ fun () ->
  let top_left = cx -. 0.5 *. w, cy +. 0.5 *. h in
  let max_l = float Image.max_l in
  let px_h = Image.h image in
  let worker_count = Fun.Async.parallel_count () in
  let worker_count, band = Fun.Async.divide_work ~size:px_h ~worker_count in
  for w = 0 to worker_count - 1 do
    Fun.Async.call_trap_exn @@ fun () ->
    let y_first, y_last = band w in
    for y = y_first to y_last do
      for x = 0 to Image.w image - 1 do
        let c = pixel_to_complex image ~x ~y ~top_left ~size in
        let l = match escape_time c ~limit:max_iteration with
        | None -> 0
        | Some t ->
            let l = float (max_iteration - t) /. float max_iteration in
            Float.to_int (max_l *. (l ** 0.4545 (* gamma correct *)))
        in
        Image.set image ~x ~y ~l;
      done
    done
  done

let write_if_some ~out_file image = match out_file with
| None -> Ok () | Some file -> Image.write_pgm file image

let mandelbrot
    ~domain_count ~out_file ~center ~size:(w, h as size) ~pixel_width
    ~max_iteration
  =
  Fun.Async.main ?domain_count @@ fun () ->
  let pixel_height = Float.(to_int @@ ceil ((h /. w) *. (float pixel_width))) in
  let image = Image.make ~w:pixel_width ~h:pixel_height ~l:Image.max_l in
  Fun.Async.get (render image ~center ~size ~max_iteration);
  match write_if_some ~out_file image with
  | Ok () -> 0 | Error e -> prerr_endline e; 1

(* Command line interface *)

open Cmdliner
open Cmdliner.Term.Syntax

let tool_cmd =
  let doc = "Render the Mandelbrot set to a PGM file" in
  Cmd.make (Cmd.info "mandelbrot" ~doc) @@
  let+ domain_count = Affect_cli.parallel_count ()
  and+ () = Affect_cli.set_parallel_trace ()
  and+ out_file =
    let doc = "$(docv) is the output PGM image file." in
    let absent = "Only compute in memory" in
    Arg.(value & pos 0 (some filepath) None &
         info [] ~doc ~docv:"FILE.pgm" ~absent)
  and+ center =
    let doc =
      "$(docv) is the complex coordinate at the center of the image."
    in
    Arg.(value & opt (pair float float) (-0.74797, -0.072500001) &
         info ["c"; "center"] ~doc ~docv:"X,Y")
  and+ size =
    let doc =
      "$(docv) is the size of the region in the complex plane."
    in
    Arg.(value & opt (pair float float) (0.005, 0.005) &
         info ["s"; "size"] ~doc ~docv:"W,H")
  and+ pixel_width =
    let doc = "$(docv) is the width of the image in pixels." in
    Arg.(value & opt int 2000 & info ["p"; "pixel-width"]~doc ~docv:"PX")
  and+ max_iteration =
    let doc = "$(docv) maximal number of iteration." in
    Arg.(value & opt int 1000 & info ["m"; "max-iteration"] ~doc ~docv:"IMAX")
  in
  mandelbrot ~domain_count ~out_file ~center ~size ~pixel_width ~max_iteration

let main () = Cmd.eval' tool_cmd
let () = if !Sys.interactive then () else exit (main ())
