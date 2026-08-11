(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** A few private tools needed for implementing affect. *)

(** {1:stdlib_woes Stdlib woes} *)

module Exn : sig
  val is_runtime_system : exn -> bool
  (** [is_runtime_system exn] is [exn] is [true] if and only if [exn] is
      [Stack_overflow], [Out_of_memory] or [Sys.break]. *)

  val trap : exn -> Printexc.raw_backtrace -> unit
end

module Atomic : sig
  include module type of Atomic

  (** Atomic backoff this is stubbed with a simple Domain.cpu_relax
      for now. Perhaps the stdlib could expose it. *)
  module Backoff : sig
    type t [@@immediate]
    val default : t
    val once : t -> t
  end
  val compare_and_exchange : 'a Atomic.t -> expect:'a -> 'a -> 'a
  val update : ('a -> 'a) -> 'a Atomic.t -> unit
  val fold_update : ('a -> 'b * 'a) -> 'a Atomic.t -> 'b
end

(** {1:adhoc Adhoc} *)

val divide_work : size:int -> worker_count:int -> int * (int -> (int * int))

(** {1:panic Panic}

    This is defined in its own module so that we can [include] it
    in Fun.Async. *)

module Panic : sig
  exception Panic of string
end

(** {1:synchronization Synchronization} *)

(** Spin locks. *)
module Spin_lock : sig
  type t
  (** The type for spin locks. *)

  val make : unit -> t
  (** [make ()] is a new spin lock. *)

  val lock : t -> unit
  (** [lock l] locks [l]. *)

  val try_lock : t -> bool
  (** [try_lock l] tries to lock [l]. *)

  val unlock : t -> unit
  (** [unlock l] unlocks [l]. *)

  val protect : t -> (unit -> 'a) -> 'a
  (** [protect l f] locks [l], executes [f ()] and makes sure [l] is unlocked
      when the function returns by value or by exception. *)
end

(** {1:datastructure Data structures} *)

(** Persistent queue.

    À la Okasaki §5.2 *)
module Persistent_queue : sig
  type 'a t
  (** The type for persistent queues holding values of type ['a]. *)

  val empty : 'a t
  (** [empty] is an empty persistent queue. *)

  val is_empty : 'a t -> bool
  (** [is_empty q] is [true] if and only if the queue has no elements. *)

  val length : 'a t -> int
  (** [length q] is the length of [q] in O(n). *)

  val add : 'a -> 'a t -> 'a t
  (** [add v q] adds [v] at the end of [q] *)

  val add' : 'a -> and_keep:('a -> bool) -> 'a t -> 'a t
  (** [add' v ~and_keep:sat q] first adds [v] at the end of [q] and then
      keeps only the [sat] satisfying elements. *)

  val untake : 'a -> 'a t -> 'a t
  (** [untake v q] adds [v] at the front of [q]. *)

  val take : 'a t -> 'a option * 'a t
  (** [take q] removes and returns the first element of [q] (if any). *)

  val take' : sat:('a -> bool) -> 'a t -> 'a option * 'a t
  (** [take q] removes and returns the first [sat] satisfying element [q]
      (if any). *)

  val peek : 'a t -> 'a option
  (** [peek q] returns the first element of [q] (if any). *)

  val drop : 'a t -> 'a t
  (** [drop q] drops the first element of [q] (if any). *)

  val peek' : sat:('a -> bool) -> 'a t -> 'a option
  (** [peek' ~sat q] returns the first [sat] satisfying element of
      [q] (if any). *)

  val drop' : sat:('a -> bool) -> 'a t -> 'a t
  (** [drop' ~sat q] removes the first [sat] satisfying element of [q]
      (if any). *)

  val iter : ('a -> unit) -> 'a t -> unit
  (** [iter f q] applies [f] to the elements of [q] in order. *)

  val keep : sat:('a -> bool) -> 'a t -> 'a t
  (** [keep ~sat q] is [q] with only those [sat] satisfying elements. *)
end

(** Synchronized queue.

    Lock-free synchronized FIFO. *)
module Synchronized_queue : sig
  type 'a t
  (** The type for synchronized queues holding values of type ['a]. *)

  val make : unit -> 'a t
  (** [make ()] is an empty queue. *)

  val is_empty : 'a t -> bool
  (** [is_empty q] is [true] if and only if the queue has no elements. *)

  val length : 'a t -> int
  (** [length q] is the length of [q] in O(n). *)

  val add : 'a -> 'a t -> unit
  (** [add v q] adds [v] at the end of [q] *)

  val add' : 'a -> and_keep:('a -> bool) -> 'a t -> unit
  (** [add v ~and_keep:sat q] first adds [v] at the end of [q] and
      then keeps only the [sat] satisfying elements. *)

  val untake : 'a -> 'a t -> unit
  (** [untake v q] adds [v] at the front of [q]. *)

  val take : 'a t -> 'a option
  (** [take q] removes and returns the first element of [q] (if any). *)

  val take' : sat:('a -> bool) -> 'a t -> 'a option
  (** [take' ~sat] removes and returns the first [sat] satisfying
      element of [q] (if any). *)

  (** {1:peeking Peeking} *)

  type 'a seen
  (** The type for a queue state seen during an operation. *)

  val peek : 'a t -> 'a option * 'a seen
  (** [peek q] returns the first element of [q] (if any) and the state of
      [q] when it was observed. *)

  val try_drop : seen:'a seen -> 'a t -> bool
  (** [try_drop ~seen q] drops the first element of [q] if [q]
      is still in the [seen] state and returns [true] on success. *)

  val peek' : sat:('a -> bool) -> 'a t -> 'a option * 'a seen
  (** [peek' ~sat q] returns the first [sat] satisfying element of
      [q] (if any) and the state of [q] when it was observed. *)

  val try_drop' : seen:'a seen -> sat:('a -> bool) -> 'a t -> bool
  (** [try_drop' ~seen ~sat q] removes from [q] the first [sat] satisfying
      element if [q] is still in the [seen] state and returns [true] on
      success. *)
end

(** Synchronized stacks.

    Lock-free synchronized LIFO. *)
module Synchronized_stack : sig
  type 'a t
  (** The type for synchronized stacks holding value of type ['a]. *)

  val make : unit -> 'a t
  (** [make ()] is an empty stack. *)

  val is_empty : 'a t -> bool
  (** [is_empty s] is [true] if and only if the stack has no elements. *)

  val length : 'a t -> int
  (** [length s] is the length of [s] in O(n). *)

  val push : 'a -> 'a t -> unit
  (** [push v s] adds [v] on the top of [s]. *)

  val pop : 'a t -> 'a option
  (** [pop s] pops the last element pushed on [s] (if any). *)
end

(** Persistent priority queues.

    Implemented as leftist heaps à la Okasaki §3.1 *)
module Persistent_pqueue : sig

  (** Output signature of the functor {!MakeMin}. *)
  module type Min = sig
    type elt
    (** The type for priority queue elements. *)

    type t
    (** The type for priority queues. *)

    val empty : t
    (** [empty] is the empty priority queue. *)

    val length : t -> int
    (** [length q] is the number of elements in [q]. O(n). *)

    val is_empty : t -> bool
    (** [is_empty q] is [true] if and only if [q] is empty. *)

    val add : elt -> t -> t
    (** [add v q] adds [v] to [q]. *)

    val min_elt : t -> elt option
    (** [min_elt q] is an element of [q] with minimal priority or [None]
        if the queue is empty. *)

    val pop_min : t -> elt option * t
    (** [pop_min q] is [q] with one of its minimal priority element
        removed. If [q] is empty this is [None, empty]. *)

    val pop_sat_or_peek_min :
      sat:(elt -> bool) -> t -> (elt, elt) Either.t option * t
    (** [pop_sat_or_peek_min ~sat q] behaves like {!pop_min} in [Either.Left]
        if the minimal priority element satsifies [sat] or {!min_elt} in
        [Either.Right] otherwise. *)

    val remove_min : t -> t
    (** [remove_min q] is [q] with one of its minimal priority element
        removed. If [q] is {!empty}, this is {!empty}. *)

    val keep : sat:(elt -> bool) -> t -> t
    (** [keep ~sat q] is [q] with only those [sat] satisfying elements. *)
  end

  module MakeMin (E : Stdlib.Pqueue.OrderedType) : Min with type elt = E.t
  (** [MakeMin (Elt)] is a min-priority queue on the elements [Elt]. *)
end

(** Synchronized priority queue.

    Lock-free priority queues. *)
module Synchronized_pqueue : sig

  (** Output signature of the functor {!MakeMin}. *)
  module type Min = sig
    type elt
    (** The type for priority queue elements. *)

    type t
    (** The type for priority queues. *)

    val make : unit -> t
    (** [make] is a new empty priority queue. *)

    val length : t -> int
    (** [length q] is the number of elements in [q]. O(n). *)

    val is_empty : t -> bool
    (** [is_empty q] is [true] if and only if [q] is empty. *)

    val add : elt -> t -> unit
    (** [add v q] adds [v] to [q]. *)

    val min_elt : t -> elt option
    (** [min_elt q] is an element of [q] with minimal priority or [None]
        if the queue is empty. *)

    val pop_min : t -> elt option
    (** [pop_min q] is [q] with one of its minimal priority element
        removed. If [q] is empty this is [None, empty]. *)

    val pop_sat_or_peek_min :
      sat:(elt -> bool) -> t -> (elt, elt) Either.t option
    (** [pop_sat_or_peek_min ~sat q] behaves like {!pop_min} in [Either.Left]
        if the minimal priority element satsifies [sat] or {!min_elt} in
        [Either.Right] otherwise. *)

    val remove_min : t -> unit
    (** [remove_min q] is [q] with one of its minimal priority element
        removed. If [q] is {!empty}, this is {!empty}. *)

    val keep : sat:(elt -> bool) -> t -> unit
    (** [keep ~sat q] keeps only in [q] those [sat] satisfying elements. *)
  end

  module MakeMin (E : Stdlib.Pqueue.OrderedType) : Min with type elt = E.t
  (** [MakeMin (Elt)] is a min-priority queue on the elements [Elt]. *)
end

(** Synchronized maps.

    As atomics over {!Stdlib.Map}. *)
module Synchronized_map : sig
  module type T = sig
    (* Incomplete, added on a per use basis. *)
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
  module Make (Map : Map.S) : T with type 'a map = 'a Map.t
                                 and type key = Map.key
end

(** Circular array (not synchronization safe) *)
module Circular_array : sig
  type index = int
  (** The type for indexes. In a circular array, there is no out of bounds
      error, indexes are taken modulo the length of the array. *)

  type 'a t
  (** The type for circular arrays holding values of type ['a].  *)

  val make : log2_length:int -> 'a -> 'a t
  (** [make ~log2_length:n v] is an array of length 2{^[n]} filled with [v] *)

  val length : 'a t -> int
  (** [length a] is the length of [a]. *)

  val get : 'a t -> index -> 'a
  (** [get a i] is the value at index [i] of [a]. *)

  val set : 'a t -> index -> 'a -> unit
  (** [set a i v] sets the value at index [i] of [a] to [v]. *)

  val grow : 'a -> first:index -> last:index -> 'a t -> 'a t
  (** [grow v ~first ~last a] increases the size of [a] to the next power
      of two. The resulting array has [v] as its elements except in
      the range [[first];[last]] where it has [a]'s values. *)

  val shrink : 'a -> first:index -> last:index -> 'a t -> 'a t
  (** [shrink ~first ~last a] decreases the size of [a] to the previous
      power of two. The resulting array has [a] as its elements except in
      the range [[first];[last]] where it has [a]'s values. *)
end

(** Work stealing queues (mostly synchronization safe).

    For a scenario where one thread only {!push}es and {!pop}s
    in LIFO order and multiple threads {!steal} in FIFO order. *)
module Work_stealing_queue : sig
  type 'a t
  (** The type for work stealing queues holding values of type ['a]. *)

  val make : nil:'a -> 'a t
  (** [make ~nil] is a new work queue, using [nil] as a stub element. *)

  val is_empty : 'a t -> bool
  (** [is_empty w] is [true] if and only if the work queue is empty. *)

  val length : 'a t -> int
  (** [length q] is the length of [q]. *)

  val push : 'a -> 'a t -> unit
  (** [push w q] pushes work [w] on [q].
      {b Warning.} Must be synchronized with [pop]. *)

  val pop : 'a t -> 'a option
  (** [pop q] pops the last element pushed on [q] (if any).
      {b Warning.} Must be synchronized with [push]. *)

  val steal : 'a t -> 'a option
  (** [steal q] steals the first element pushed on [q] (if any). *)
end

(** {1:unused Unused} *)

(** Synchronized sets.

    As atomics over {!Stdlib.Set}. *)
module Synchronized_set : sig
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
  module Make (Set : Set.S) : T with type set = Set.t
                                 and type elt = Set.elt
end

(** Circular doubly linked lists (not synchronization safe). *)
module Circular_list : sig
  type 'a t
  (** The type for circular lists holding values of type ['a]. *)

  val make : unit -> 'a t
  (** [make ()] is a new list. *)

  val add_first : 'a t -> 'a -> unit
  (** [add_first l v] adds [v] at the front of the list. *)

  val add_last : 'a t -> 'a -> unit
  (** [add_last l v] adds [v] at the end of the list. *)

  val take_first : 'a t -> 'a option
  (** [take_first l] is the first element of [l] (if any). *)

  val take_last : 'a t -> 'a option
  (** [take_last l] is the last element of [l] (if any). *)

  val take : sat:('a -> bool) -> 'a t -> 'a option
  (** [take ~sat] is the first element satisfying [sat]. *)

  val of_list : 'a list -> 'a t
  (** [of_list l] is a circular list with elements [l]. *)
end
