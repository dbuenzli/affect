(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let strf = Printf.sprintf

(* Stdlib woes *)

module Exn = struct
  let is_runtime_system = function
  | Stack_overflow | Out_of_memory | Sys.Break -> true | _ -> false

  let trap exn bt = Printexc.default_uncaught_exception_handler exn bt
  (* XXX That's of course not what we want, but there's no way to access the
     uncaught exception handler. The stdlib is still missing something,
     but we'd rather not have affect expose its own trap. It would
     be good to eventually get a global exception trap, the whole uncaught
     exception handling remains a total undesigned mess upstream
     see https://github.com/ocaml/ocaml/issues/11074#issuecomment-1099998167 *)
end

module Atomic = struct
  include Atomic

  module Backoff = struct
    (* XXX This is is a stub, it must be likely replaced by something more
       sophisticated. See e.g. what went into Stdlib's Atomic. We have it here
       as its use affects the code structure. We should start petitioning
       upstream and smart people to expose something sensitive, since not all
       backoffs can be conveniently abstracted by update combinators. *)
    type t = int
    let default = 1
    let[@inline never] once backoff =
      for i = 0 to backoff do Domain.cpu_relax () done;
      backoff * 2
  end

  let compare_and_exchange a ~expect desired =
    (* See https://github.com/ocaml/ocaml/issues/14889 *)
    let rec loop ~backoff a ~expect desired =
      let current = Atomic.get a in
      if not (current == expect) then current else
      if Atomic.compare_and_set a expect desired then expect else
      loop ~backoff:(Backoff.once backoff) a ~expect desired
    in
    loop ~backoff:Backoff.default a ~expect desired

  let update f a = (* available in OCaml >= 5.6 *)
    let rec loop ~backoff f a =
      let old = Atomic.get a in
      let new' = f old in
      if old == new' || Atomic.compare_and_set a old new' then () else
      loop ~backoff:(Backoff.once backoff) f a
    in
    loop ~backoff:Backoff.default f a

  let fold_update f a =
    (* https://github.com/ocaml/ocaml/pull/14725 *)
    let rec loop ~backoff f a =
      let old = Atomic.get a in
      let (acc, new') = f old in
      if old == new' || Atomic.compare_and_set a old new' then acc else
      loop ~backoff:(Backoff.once backoff) f a
    in
    loop ~backoff:Backoff.default f a
end

(* Adhoc *)

let err_worker_count c = strf "worker_count %d not greater than 0" c
let err_work_size n = strf "work size %d is negative" n
let err_worker_index i worker_count =
  if worker_count = 0
  then strf "no ranges: work size was 0"
  else strf "worker index %d not in range [0;%d]" i (worker_count - 1)

let divide_work ~size ~worker_count =
  if worker_count < 1 then invalid_arg (err_worker_count worker_count);
  if size < 0 then invalid_arg (err_work_size size);
  if size = 0 then (0, fun i -> invalid_arg (err_worker_index i 0)) else
  let worker_count = Int.min size worker_count in
  let batch_size = size / worker_count in
  let batch_rem = size mod worker_count in
  let range workeri =
    if workeri < 0 || workeri >= worker_count
    then invalid_arg (err_worker_index workeri worker_count);
    let first = workeri * batch_size + Int.min workeri batch_rem in
    let last = first + batch_size - 1 + (if workeri < batch_rem then 1 else 0)in
    (first, last)
  in
  worker_count, range

(* Panic *)

module Panic = struct (* in a module so that we can [include] it in Fun.Async *)
  exception Panic of string

  let print_panic = function
  | Panic e -> Some (strf "Fun.Async.Panic:\n%s" e)
  | _ -> None

  let () = Printexc.register_printer print_panic
end

(* Synchronization *)

module Spin_lock = struct
  type t = bool Atomic.t

  let make () = Atomic.make false
  let lock l =
    let rec loop ~backoff l =
      if Atomic.compare_and_set l false true then () else
      loop ~backoff:(Atomic.Backoff.once backoff) l
    in
    loop ~backoff:Atomic.Backoff.default l

  let try_lock l = Atomic.compare_and_set l false true
  let[@inline] unlock l = Atomic.set l false
  let protect l f =
    lock l;
    match f () with
    | v -> unlock l; v
    | exception exn ->
        let bt = Printexc.get_raw_backtrace () in
        unlock l; Printexc.raise_with_backtrace exn bt
end

(* Data structures *)

module Persistent_queue = struct
  let list_find_remove ~sat l =
    let rec loop sat acc = function
    | v :: vs when sat v -> Some (v, List.rev_append acc vs)
    | v :: vs -> loop sat (v :: acc) vs
    | [] -> None
    in
    loop sat [] l

  (* Persistent queue à la Okasaki §5.2 with a bit of non-optimal deque *)

  type 'a t = (* invariant: front is never empty if the queue is not *)
    { front : 'a list; (* head of the list is the first queue element *)
      rear : 'a list (* head of the list is the last queue element *) }

  let empty = { front = []; rear = [] }
  let is_empty q = List.is_empty q.front
  let length q = List.length q.front + List.length q.rear

  let ensure_front q = (* invariant: front is never empty if the queue is not *)
    if not (List.is_empty q.front) then q else
    { front = List.rev q.rear; rear = [] }

  let add v q = ensure_front { q with rear = v :: q.rear }
  let add' v ~and_keep:sat q =
    let front = List.filter sat q.front in
    let rear = List.filter sat (v :: q.rear) in
    ensure_front { front; rear; }

  let untake v q = { q with front = v :: q.front }

  let take q =
    if List.is_empty q.front then None, q else
    Some (List.hd q.front), ensure_front { q with front = List.tl q.front }

  let take' ~sat q =
    if List.is_empty q.front then None, q else
    let v = List.hd q.front in
    if sat v then Some v, ensure_front {q with front = List.tl q.front } else
    match list_find_remove ~sat q.front with
    | Some (v, front) -> Some v, ensure_front { q with front }
    | None ->
        match list_find_remove ~sat (List.rev q.rear) with
        | None -> None, q
        | Some (v, rev_rear) -> Some v, { q with rear = List.rev rev_rear }

  let peek q =
    if List.is_empty q.front then None else Some (List.hd q.front)

  let drop q =
    if List.is_empty q.front then q else
    ensure_front { q with front = List.tl q.front }

  let peek' ~sat q =
    if List.is_empty q.front then None else
    let v = List.hd q.front in
    if sat v then Some v else
    match List.find_opt sat q.front with
    | Some _ as v -> v
    | None -> List.find_opt sat (List.rev q.rear)

  let drop' ~sat q =
    if List.is_empty q.front then q else
    let v = List.hd q.front in
    if sat v then ensure_front { q with front = List.tl q.front } else
    match list_find_remove ~sat q.front with
    | Some (_, front) -> ensure_front { q with front }
    | None ->
        match list_find_remove ~sat (List.rev q.rear) with
        | None -> q
        | Some (_, rev_rear) -> { q with rear = List.rev rev_rear }

  let iter f q = List.iter f q.front; List.iter f (List.rev q.rear)

  let keep ~sat q =
    let front = List.filter sat q.front in
    let rear = List.filter sat q.rear in
    ensure_front { front; rear }

  let take_last q = match q.rear with (* not optimal, not exposed in the API *)
  | v :: rear -> Some v, { q with rear }
  | [] ->
      if List.is_empty q.front then None, q else
      let rev_front = List.rev q.front in
      Some (List.hd rev_front), { q with front = List.rev (List.tl rev_front)}
end

module Synchronized_queue = struct
  type 'a t = 'a Persistent_queue.t Atomic.t

  let make () = Atomic.make_contended (Persistent_queue.empty)
  let is_empty q = Persistent_queue.is_empty (Atomic.get q)
  let length q = Persistent_queue.length (Atomic.get q)
  let add v q = Atomic.update (Persistent_queue.add v) q
  let add' v ~and_keep q = Atomic.update (Persistent_queue.add' v ~and_keep) q
  let untake v q = Atomic.update (Persistent_queue.untake v) q
  let take q = Atomic.fold_update Persistent_queue.take q
  let take' ~sat q = Atomic.fold_update (Persistent_queue.take' ~sat) q
  let take_last q = Atomic.fold_update Persistent_queue.take_last q

  (* Peeking *)

  type 'a seen = 'a Persistent_queue.t

  let peek q =
    let seen = Atomic.get q in
    Persistent_queue.peek seen, seen

  let try_drop ~seen q =
    let new' = Persistent_queue.drop (Atomic.get q) in
    Atomic.compare_and_set q seen new'

  let peek' ~sat q =
    let seen = Atomic.get q in
    Persistent_queue.peek' ~sat seen, seen

  let try_drop' ~seen ~sat q =
    let new' = Persistent_queue.drop' ~sat (Atomic.get q) in
    Atomic.compare_and_set q seen new'
end

module Synchronized_stack = struct
  (* Atomic over a persistent list used as a stack *)
  type 'a t = 'a list Atomic.t

  let make () = Atomic.make_contended []
  let is_empty s = List.is_empty (Atomic.get s)
  let length s = List.length (Atomic.get s)
  let push v s = Atomic.update (List.cons v) s
  let pop s =
    let rec loop ~backoff s = match Atomic.get s with
    | [] -> None
    | v :: new' as old ->
        if Atomic.compare_and_set s old new' then Some v else
        loop ~backoff:(Atomic.Backoff.once backoff) s
    in
    loop ~backoff:Atomic.Backoff.default s
end

module Persistent_pqueue = struct
  module type Min = sig
    type elt
    type t
    val empty : t
    val length : t -> int
    val is_empty : t -> bool
    val add : elt -> t -> t
    val min_elt : t -> elt option
    val pop_min : t -> elt option * t
    val pop_sat_or_peek_min :
      sat:(elt -> bool) -> t -> (elt, elt) Either.t option * t

    val remove_min : t -> t
    val keep : sat:(elt -> bool) -> t -> t
  end
  module MakeMin (E : Stdlib.Pqueue.OrderedType) = struct
    (* Leftist heap à la Okasaki §5.2  *)
    type rank = int (* rank of the right spine *)
    type elt = E.t
    type t = Empty | Node of rank * E.t * t * t

    let empty = Empty
    let singleton v = Node (1, v, Empty, Empty)
    let is_empty = function Empty -> true | _ -> false
    let rec length = function
    | Empty -> 0 | Node (_, _, l, r) -> 1 + length l + length r

    let rank = function Empty -> 0 | Node (rank, _, _, _) -> rank
    let make_node v n0 n1 =
      let rank_n0 = rank n0 and rank_n1 = rank n1 in
      if rank n0 > rank n1
      then Node (rank_n1 + 1, v, n0, n1)
      else Node (rank_n0 + 1, v, n1, n0)

    let rec merge t0 t1 = match t0, t1 with
    | Empty, t | t, Empty -> t
    | Node (_, v0, l0, r0), Node (_, v1, l1, r1) ->
        if E.compare v0 v1 < 0
        then make_node v0 l0 (merge r0 t1)
        else make_node v1 l1 (merge t0 r1)

    let add v t = merge (singleton v) t
    let min_elt = function Empty -> None | Node (_, v, _, _) -> Some v
    let pop_min = function
    | Empty -> None, empty
    | Node (_, v, l, r) -> Some v, merge l r

    let pop_sat_or_peek_min ~sat = function
    | Empty -> None, empty
    | Node (_, v, l, r) when sat v -> Some (Either.Left v), merge l r
    | Node (_, v, _, _) as t -> Some (Either.Right v), t

    let remove_min = function Empty -> Empty | Node (_, _, l, r) -> merge l r

    let rec keep ~sat = function
    | Empty -> Empty
    | Node (_, v, l, r) ->
        let l = keep ~sat l in
        let r = keep ~sat r in
        if sat v then add v (merge l r) else (merge l r)
  end
end

module Synchronized_pqueue = struct
  module type Min = sig
    type elt
    type t
    val make : unit -> t
    val length : t -> int
    val is_empty : t -> bool
    val add : elt -> t -> unit
    val min_elt : t -> elt option
    val pop_min : t -> elt option
    val pop_sat_or_peek_min :
      sat:(elt -> bool) -> t -> (elt, elt) Either.t option

    val remove_min : t -> unit
    val keep : sat:(elt -> bool) -> t -> unit
  end
  module MakeMin (E : Stdlib.Pqueue.OrderedType) = struct
    module Persistent_pqueue = Persistent_pqueue.MakeMin (E)
    type elt = E.t
    type t = Persistent_pqueue.t Atomic.t
    let make () = Atomic.make Persistent_pqueue.empty
    let length q = Persistent_pqueue.length (Atomic.get q)
    let is_empty q = Persistent_pqueue.is_empty (Atomic.get q)
    let add v q = Atomic.update (Persistent_pqueue.add v) q
    let min_elt q = Persistent_pqueue.min_elt (Atomic.get q)
    let pop_min q = Atomic.fold_update Persistent_pqueue.pop_min q
    let pop_sat_or_peek_min ~sat q =
      Atomic.fold_update (Persistent_pqueue.pop_sat_or_peek_min ~sat) q
    let remove_min q = Atomic.update Persistent_pqueue.remove_min q
    let keep ~sat q = Atomic.update (Persistent_pqueue.keep ~sat) q
  end
end

module Synchronized_map = struct
  module type T = sig
    (* Incomplete, for now added on a per use basis. *)
    type key
    type 'a map
    type 'a t
    val make : unit -> 'a t
    val get_map : 'a t -> 'a map
    val set_map : 'a t -> 'a map -> unit
    val is_empty : 'a t -> bool
    val add_to_list : key -> 'a -> 'a list t -> unit
    val update : key -> ('a option -> 'a option) -> 'a t -> unit
    val update_all : (key -> 'a -> 'a option) -> 'a t -> unit
    val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val find_and_remove : key -> 'a t -> 'a option
  end
  module Make (Map : Map.S) = struct
    type key = Map.key
    type 'a map = 'a Map.t
    type 'a t = 'a Map.t Atomic.t
    let make () = Atomic.make_contended Map.empty
    let get_map m = Atomic.get m
    let set_map m pm = Atomic.set m pm
    let is_empty m = Map.is_empty (Atomic.get m)
    let add_to_list k v m = Atomic.update (Map.add_to_list k v) m
    let update k v m = Atomic.update (Map.update k v) m
    let update_all f m = Atomic.update (Map.filter_map f) m
    let fold f m acc = Map.fold f (Atomic.get m) acc
    let find_and_remove k m =
      (* Map should have that function *)
      let find_and_remove k m = match Map.find_opt k m with
      | None -> None, m
      | Some _ as v -> v, Map.remove k m
      in
      Atomic.fold_update (find_and_remove k) m
  end
end

module Circular_array = struct
  type index = int
  type 'a t = 'a array (* length is power of 2 for cheap modulo with [land] *)

  let make ~log2_length v = Array.make (1 lsl log2_length) v
  let length a = Array.length a
  let[@inline] index a i = i land (Array.length a - 1)
  let[@inline] get a i = Array.get a (index a i)
  let[@inline] set a i v = Array.set a (index a i) v

  let copy ~src ~first ~last ~dst =
    for i = first to last do set dst i (get src i) done

  let grow v ~first ~last a =
    let new' = Array.make (Array.length a lsl 1) v in
    copy ~src:a ~first ~last ~dst:new'; new'

  let shrink v ~first ~last a =
    let new' = Array.make (Array.length a lsr 1) v in
    copy ~src:a ~first ~last ~dst:new'; new'
end

module Work_stealing_queue_persistent = struct
  (* This is a simple implementation that uses the current non optimal
     implementation of [take_last]. It's not in use, it was used
     when we had Work_stealing_queue_chase_lev suspicions. *)

  type 'a t = 'a Synchronized_queue.t
  let make ~nil:_ = Synchronized_queue.make ()
  let is_empty = Synchronized_queue.is_empty
  let length = Synchronized_queue.length
  let push = Synchronized_queue.add
  let pop = Synchronized_queue.take_last (* Not optimal *)
  let steal = Synchronized_queue.take
end

module Work_stealing_queue_chase_lev = struct
  (* This is a basic implementation of _Dynamic circular work-stealing
     deque_ Chase & Lev <https://doi.org/10.1145/1073970.1073974>
     Could be improved, not all the tricks are implemented (e.g. §2.3,
     §3.1).

     For cross-checking see also Fig 1. of _Correct and Efficient Work-Stealing
     for Weak Memory Models_ Lê et al. <https://doi.org/10.1145/2442516.2442524>
     Note in passing the funny bug noticed by <https://wingolog.org/archives/\
     2022/10/03/on-correct-and-efficient-work-stealing-for-weak-memory-models>.

     The whole paper relies on [int] increments not overflowing
     which is likely unreasonable for a 32-bit platform but the latter is
     dying for OCaml.

     We use [next_right] for what is [bottom] in the paper and [left] for
     what is [top] as our brain finds it easier to follow the code that
     way. We use the variable [len] to denote the number of elements in the
     queue which is [next_right - left] and [right] for [next_right - 1].

     [next_right] is conceptually on the right of the array and indicates
     the next available array element to push work in. It is incremented on
     [push] operations.

     [left] is conceptually on the left of the array and indicates the first
     array element where work can be stealed unless the queue is empty. It is
     incremented on [steal] operations.

     Above we say "conceptually" since as we are using a circular array,
     the right may, concretely, be on the left of the underlying array.

     The atomics for [next_right] and [left] are created with
     [Atomic.make_contented] to avoid stealers and the work pusher to mutually
     invalidate their caches.

     In the work array we store references on work items. This is to be able
     to clear the elements of the array after we [pop]/[steal] them. Acting
     directly on the array element to nillify the place where we took them
     from after the [compare_and_set] operations wouldn't be correct as these
     may already have been updated concurrently with new values. On the other
     hand we need to clear these values so that the gc doesn't keep a
     reference on these. *)

  type 'a t =
    { nil : 'a ref; (* Stub value for empty cells *)
      left : int Atomic.t;
      next_right : int Atomic.t;
      mutable work : 'a ref Circular_array.t }

  let init_log2_capacity = 4
  let init_capacity = 1 lsl init_log2_capacity

  let make ~nil =
    let nil = ref nil in
    let left = Atomic.make_contended 0 in
    let next_right = Atomic.make_contended 0 in
    let work = Circular_array.make ~log2_length:init_log2_capacity nil in
    { nil; left; next_right; work }

  let length q = Atomic.get q.next_right - Atomic.get q.left
  let is_empty q = length q <= 0
  let capacity q = Circular_array.length q.work
  let[@inline] deref_and_clear q wref = let w = !wref in wref := !(q.nil); w

  let grow q ~left ~right =
    q.work <- Circular_array.grow q.nil ~first:left ~last:right q.work

  let perhaps_shrink q ~left ~right =
    let len = right - left + 1 and capacity = capacity q in
    if capacity > init_capacity && 3 * len < capacity
    then q.work <- Circular_array.shrink q.nil ~first:left ~last:right q.work

  let push w q =
    let next_right = Atomic.get q.next_right in
    let left = Atomic.get q.left in
    let len = next_right - left in
    (if len + 1 > capacity q then grow q ~left ~right:(next_right - 1));
    Circular_array.set q.work next_right (ref w);
    Atomic.incr q.next_right

  let pop q =
    let right = Atomic.fetch_and_add q.next_right (-1) - 1 in
    let left = Atomic.get q.left in
    let len = right - left + 1 in
    if len <= 0 then (Atomic.incr q.next_right; None) else
    let wref = Circular_array.get q.work right in
    if len > 1
    then (perhaps_shrink q ~left ~right; Some (deref_and_clear q wref)) else
    if Atomic.compare_and_set q.left left (left + 1)
    then (Atomic.incr q.next_right; Some (deref_and_clear q wref))
    else (Atomic.incr q.next_right; None)

  let steal q =
    let rec loop ~backoff q =
      let left = Atomic.get q.left in
      let next_right = Atomic.get q.next_right in
      let len = next_right - left in
      if len <= 0 then None else
      let wref = Circular_array.get q.work left in
      if Atomic.compare_and_set q.left left (left + 1)
      then Some (deref_and_clear q wref)
      else loop ~backoff:(Atomic.Backoff.once backoff) q
    in
    loop ~backoff:Atomic.Backoff.default q
end

module Work_stealing_queue = struct
  (* XXX It would be nice to get an assement of the difference between that
     tricky and a bit ugly data structure and a more optimal
     Work_stealing_queue_persisent. Of course persistent data
     structures allocate a more but then that one does aswell due to
     reference indirection. *)
  (* include Work_stealing_queue_persistent *)
  include Work_stealing_queue_chase_lev
end

(* Unused code for now *)

module Synchronized_set = struct
  module type T = sig
    (* Incomplete, added on a per use basis. *)
    type elt
    type set
    type t
    val make : unit -> t
    val get_set : t -> set
    val set_set : t -> set -> unit
    val add : elt -> t -> unit
    val remove : elt -> t -> unit
    val is_empty : t -> bool
  end
  module Make (Set : Set.S) = struct
    type elt = Set.elt
    type set = Set.t
    type t = Set.t Atomic.t
    let make () = Atomic.make Set.empty
    let get_set s = Atomic.get s
    let set_set s ps = Atomic.set s ps
    let add v s = Atomic.update (Set.add v) s
    let remove v s = Atomic.update (Set.remove v) s
    let is_empty s = Set.is_empty (Atomic.get s)
  end
end

module Circular_list = struct
  type 'a t =
    { mutable v : 'a option; (* None is for the root. *)
      mutable prev : 'a t; (* on root this points to last element. *)
      mutable next : 'a t; (* on root this points to the first element. *) }

  let make () = let rec root = { v = None; next = root; prev = root } in root

  let make_first root n =
    n.next.prev <- n.prev; n.prev.next <- n.next;
    n.next <- root.next; n.prev <- root;
    root.next.prev <- n; root.next <- n

  let add_first root v =
    let n = { v = Some v; prev = root; next = root.next } in
    root.next.prev <- n; root.next <- n

  let add_last root v =
    let n = { v = Some v; prev = root.prev; next = root } in
    root.prev.next <- n; root.prev <- n

  let take_first root =
    let first = root.next in
    root.next <- first.next; first.next.prev <- root; first.v

  let take_last root =
    let last = root.prev in
    root.prev <- last.prev; last.prev.next <- root; last.v

  let take ~sat root = (* O(n) *)
    let rec loop pred n = match n.v with
    | None -> None
    | Some v when sat v -> n.next.prev <- n.prev; n.prev.next <- n.next; n.v
    | Some _ -> loop pred n.next
    in
    loop pred root.next

  let of_list vs = let l = make () in List.iter (add_last l) vs; l
end
