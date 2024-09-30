(*---------------------------------------------------------------------------
   Copyright (c) 2022 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let uerror e = Unix.error_message e
let fmt_error fmt = Format.kasprintf (fun s -> Error s) fmt
let string_subrange ?(first = 0) ?last s =
  let max = String.length s - 1 in
  let last = match last with
  | None -> max
  | Some l when l > max -> max
  | Some l -> l
  in
  let first = if first < 0 then 0 else first in
  if first > last then "" else
  String.sub s first (last - first + 1)

module Signal = struct
  type t = int

  let set s b = match Sys.signal s b with
  | b -> Ok b | exception Sys_error e -> Error e

  let set_noerr s b = try Sys.set_signal s b with Sys_error _ -> ()

  let with' s b f = match set s b with
  | Error _ as e -> e
  | Ok old_b ->
      let finally () = set_noerr s old_b in
      Ok (Fun.protect ~finally f)
end

(* Sleeping *)

module Sleep = struct

  (* Time, using monotonic time would be better here but let's just
     rely on Unix. Mtime_clock.now should be added to Unix. *)

  type time_s = float (* absolute time *)
  let time_now_s () = Unix.gettimeofday ()
  let is_earlier t ~than = Float.compare t than < 0

  (* Sleeps *)

  type t = { until : time_s; mutable fiber : Fiber.E.t option }
  let forever = { until = max_float; fiber = None }
  let for' ~dur_s fiber = { until = time_now_s () +. dur_s; fiber}

  (* Heap priority queue, classical imperative implementation. *)

  let heap_compare h i i' = Float.compare h.(i).until h.(i').until
  let heap_swap h i i' = let v = h.(i) in h.(i) <- h.(i'); h.(i') <- v

  let rec heap_up h i =
    if i = 0 then () else
    let p = (i - 1) / 2 in (* parent index. *)
    if heap_compare h i p < 0 then (heap_swap h i p; heap_up h p)

  let rec heap_down h max i =
    let start = 2 * i in
    let l = start + 1 in (* left child index. *)
    let r = start + 2 in (* right child index. *)
    if l > max then () (* no child, stop *) else (* find smallest child k. *)
    let k = if r > max then l else (if heap_compare h l r < 0 then l else r) in
    if heap_compare h i k > 0 then (heap_swap h i k; heap_down h max k)

  type heap =
    { mutable sleeps : t array; (* Heap priority queue for sleeps. *)
      mutable max : int (* Index of last element of [sleeps]. *) }

  let heap_array () = Array.make 256 forever
  let heap () = { sleeps = heap_array (); max = -1 }
  let shrink_threshold = 262144
  let shrink h = (* assert (s.max < 0). *)
    if Array.length h.sleeps < shrink_threshold then () else
    h.sleeps <- heap_array ()

  let grow h =
    let len = h.max + 1 in
    let els' = Array.make (2 * len) forever in
    Array.blit h.sleeps 0 els' 0 len; h.sleeps <- els'

  let add h s =
    let max = h.max + 1 in
    if max = Array.length h.sleeps then grow h;
    h.max <- max;
    h.sleeps.(h.max) <- s; heap_up h.sleeps h.max

  let pop h =
    let last = h.sleeps.(h.max) in
    h.sleeps.(h.max) <- forever;
    h.max <- h.max - 1;
    if h.max < 0 then shrink h else
    (h.sleeps.(0) <- last; heap_down h.sleeps h.max 0)

  let dur_s_to_next_wakeup h =
    let rec loop h now =
      if h.max < 0 then None else
      if h.sleeps.(0).fiber = None then (pop h; loop h now) else
      let until = h.sleeps.(0).until in
      let late = is_earlier until ~than:now in
      Some (if late then 0. else (until -. now))
    in
    loop h (time_now_s ())

  let wakeup h =
    let rec loop h now =
      if h.max < 0 then None else
      if h.sleeps.(0).fiber = None then (pop h; loop h now) else
      let until = h.sleeps.(0).until in
      if not (is_earlier until ~than:now) then None else
      let fiber = h.sleeps.(0).fiber in
      pop h; fiber
    in
    loop h (time_now_s ())
end

(* Per domain blocking data structure  *)

module Fmap = Map.Make (Fiber.E)
type blocked =
  { mutable read : Unix.file_descr Fmap.t;
    mutable write : Unix.file_descr Fmap.t;
    fd_ready : Fiber.E.t Queue.t;
    sleeps : Sleep.heap; }

let blocked =
  let blocked_make () =
    { read = Fmap.empty; write = Fmap.empty;
      fd_ready = Queue.create (); sleeps = Sleep.heap ();  }
  in
  let blocked = Domain.DLS.new_key blocked_make in
  fun () -> Domain.DLS.get blocked

(* Blocking and unblocking *)

let block_read fd =
  let b = blocked () in
  let block fd f = b.read <- Fmap.add f fd b.read in
  let abort f = b.read <- Fmap.remove f b.read in
  let retv _ = () in
  Fiber.block ~block:(block fd) ~abort ~retv

let block_write fd =
  let b = blocked () in
  let block fd f = b.write <- Fmap.add f fd b.write in
  let abort f = b.write <- Fmap.remove f b.write in
  let retv _ = () in
  Fiber.block ~block:(block fd) ~abort ~retv

let sleep_s dur_s =
  let b = blocked () in
  (* A bit ugly, better Fiber.block ?  *)
  let s = Sleep.for' ~dur_s None in
  let block f = s.Sleep.fiber <- Some f; Sleep.add b.sleeps s in
  let abort f = s.Sleep.fiber <- None in
  let retv _ = () in
  Fiber.block ~block ~abort ~retv

let wait ~poll b =
  let timeout =
    if poll then 0. else match Sleep.dur_s_to_next_wakeup b.sleeps with
    | None -> -1.
    | Some dur_s -> dur_s
  in
  let add_fd f fd acc = fd :: acc in
  let rset = Fmap.fold add_fd b.read [] in
  let wset = Fmap.fold add_fd b.write [] in
  if rset = [] && wset = [] && timeout <= 0. then Sleep.wakeup b.sleeps else
  match Unix.select rset wset [] timeout with
  | exception Unix.(Unix_error (EINTR, _, _)) -> None
  | rset, wset, _ ->
      if rset = [] && wset = [] then Sleep.wakeup b.sleeps else
      let upd xset f fd = match List.mem fd xset with
      | true -> Queue.add f b.fd_ready; None | false -> Some fd
      in
      (if rset <> [] then b.read <- Fmap.filter_map (upd rset) b.read);
      (if wset <> [] then b.write <- Fmap.filter_map (upd wset) b.write);
      Queue.take_opt b.fd_ready

let unblock ~poll =
  let b = blocked () in
  match Queue.take_opt b.fd_ready with
  | Some _ as f -> f
  | None ->
      match Sleep.wakeup b.sleeps with
      | Some _ as f -> f
      | None -> wait ~poll b

(* File descriptor operations *)

let rec read fd b ~start ~len = match Unix.read fd b start len with
| exception Unix.(Unix_error (EINTR, _, _)) -> read fd b ~start ~len
| exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
    block_read fd; read fd b ~start ~len
| 0 when len <> 0 -> false
| c when c < len -> read fd b ~start:(start + c) ~len:(len - c)
| _ -> true

let rec write fd b ~start ~len = match Unix.single_write fd b start len with
| exception Unix.(Unix_error (EINTR, _, _)) -> write fd b ~start ~len
| exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
    block_write fd; write fd b ~start ~len
| c when c < len -> write fd b ~start:(start + c) ~len:(len - c)
| _ -> ()

let rec accept ?cloexec fd = match Unix.accept ?cloexec fd with
| (fd, _ as ret) -> Unix.set_nonblock fd; ret
| exception Unix.(Unix_error (EINTR, _, _)) -> accept ?cloexec fd
| exception Unix.(Unix_error ((EWOULDBLOCK | EAGAIN), _, _)) ->
    block_read fd; accept ?cloexec fd

let rec connect fd addr = match Unix.connect fd addr with
| () -> ()
| exception Unix.(Unix_error (EINTR, _, _)) -> connect fd addr
| exception Unix.(Unix_error (EINPROGRESS, _, _)) ->
    block_write fd;
    match Unix.getsockopt_error fd with
    | None -> ()
    | Some error -> raise (Unix.Unix_error (error, "connect", ""))

let close_noerr fd = try Unix.close fd with Unix.Unix_error _ -> ()

(* Socket endpoints *)

type endpoint =
[ `Host of string * int
| `Sockaddr of Unix.sockaddr
| `Fd of Unix.file_descr ]

let endpoint_of_string ~default_port s =
  match String.contains s Filename.dir_sep.[0] with
  | true -> Ok (`Sockaddr (Unix.ADDR_UNIX s))
  | false ->
      match String.rindex_opt s ':' with
      | None -> Ok (`Host (s, default_port))
      | Some i ->
          match String.index_from_opt s i ']' with (* beware IPv6 *)
          | Some _ -> Ok (`Host (s, default_port))
          | None ->
              let h = string_subrange ~last:(i - 1) s in
              let p = string_subrange ~first:(i + 1) s in
              match int_of_string_opt p with
              | None -> fmt_error "port %S not an integer" p
              | Some p -> Ok (`Host (h, p))

let pp_endpoint ppf ep =
  let pp_name_port ppf (n, p) = Format.fprintf ppf "%s:%d" n p in
  match ep with
  | `Host (n, p) -> pp_name_port ppf (n, p)
  | `Fd _fd -> Format.fprintf ppf "<fd>"
  | `Sockaddr (Unix.ADDR_UNIX s) -> Format.pp_print_string ppf s
  | `Sockaddr (Unix.ADDR_INET (a, p)) ->
      pp_name_port ppf (Unix.string_of_inet_addr a, p)

let rec socket_of_endpoint ep stype = match ep with
| `Fd fd -> Ok (None, fd, false)
| `Host (name, port) ->
    begin match Unix.gethostbyname name with
    | exception Not_found -> fmt_error "%s: host not found" name
    | h ->
        let c = `Sockaddr (Unix.ADDR_INET (h.h_addr_list.(0), port)) in
        socket_of_endpoint c stype
    end
| `Sockaddr addr ->
    let domain = Unix.domain_of_sockaddr addr in
    match Unix.socket ~cloexec:true domain stype 0 with
    | exception Unix.Unix_error (e, _, _) -> Error (uerror e)
    | fd ->
        match Unix.set_nonblock fd with
        | exception Unix.Unix_error (e, _, _) ->
            close_noerr fd; Error (uerror e)
        | () -> Ok (Some addr, fd, true)
