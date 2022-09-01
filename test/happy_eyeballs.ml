
let happy_eyeballs :
    delay_s:float -> tries:(unit -> 'a option) list -> discard:('a -> unit) ->
    'a option
=
fun ~delay_s ~tries ~discard ->
  let either fst snd = match Option.join fst with
  | None -> Option.join (Fiber.join snd)
  | Some _ as v ->
      Fiber.abort snd; Option.iter discard (Option.join (Fiber.join snd)); v
  in
  let rec loop = function
  | [] -> None
  | t :: ts ->
      let t = Fiber.spawn t in
      let ts = Fiber.spawn (fun () -> Funix.sleep_s delay_s; loop ts) in
      match Fiber.first t ts with
      | Either.Left t -> either t ts
      | Either.Right ts -> either ts t
  in
  loop tries


let happy_eyeballs' :
    delay_s:float -> tries:(unit -> 'a option) list -> discard:('a -> unit) ->
    'a option
=
fun ~delay_s ~tries ~discard ->
  let either fst snd = match Option.join fst with
  | None -> Option.join (Fiber.join snd)
  | Some _ as v ->
      Fiber.abort snd; Option.iter discard (Option.join (Fiber.join snd)); v
  in
  let tries = Queue.of_seq (List.to_seq tries) in
  let rec next () = match Queue.take_opt tries with
  | None -> None
  | Some t ->
      let attempt () = match t () with None -> next () | Some _ as v -> v in
      let t = Fiber.spawn attempt in
      let ts = Fiber.spawn (fun () -> Funix.sleep_s delay_s; next ()) in
      match Fiber.first t ts with
      | Either.Left t -> either t ts
      | Either.Right ts -> either ts t
  in
  next ()

let happy_eyeballs'' :
    delay_s:float -> tries:(unit -> 'a option) list -> discard:('a -> unit) ->
    'a option
=
fun ~delay_s ~tries ~discard ->
  let either fst snd = match Option.join fst with
  | None -> Option.join (Fiber.join snd)
  | Some _ as v ->
      Fiber.abort snd; Option.iter discard (Option.join (Fiber.join snd)); v
  in
  let tries = Queue.of_seq (List.to_seq tries) in
  let rec seqs () = match Queue.take_opt tries with
  | None -> None
  | Some t ->
      let rec seq t = match t () with
      | Some _ as v -> v
      | None ->
          match Queue.take_opt tries with
          | None -> None | Some t -> seq t
      in
      let t = Fiber.spawn (fun () -> seq t) in
      let ts = Fiber.spawn (fun () -> Funix.sleep_s delay_s; seqs ()) in
      match Fiber.first t ts with
      | Either.Left t -> either t ts
      | Either.Right ts -> either ts t
  in
  seqs ()

let happy_eyeballs''' :
    delay_s:float -> tries:(unit -> 'a option) list -> discard:('a -> unit) ->
    'a option
=
fun ~delay_s ~tries ~discard ->
  let either fst snd = match Option.join fst with
  | None -> Option.join (Fiber.join snd)
  | Some _ as v ->
      Fiber.abort snd; Option.iter discard (Option.join (Fiber.join snd)); v
  in
  let tries = Queue.of_seq (List.to_seq tries) in
  let rec seq () = match Queue.take_opt tries with
  | None -> None
  | Some t -> match t () with None -> seq () | Some _ as v -> v
  in
  let rec seqs () =
    if Queue.is_empty tries then None else
    let t = Fiber.spawn seq in
    let ts = Fiber.spawn (fun () -> Funix.sleep_s delay_s; seqs ()) in
    match Fiber.first t ts with
    | Either.Left t -> either t ts
    | Either.Right ts -> either ts t
  in
  seqs ()
