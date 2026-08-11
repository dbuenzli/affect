(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** First-class synchronous actions.

    Actions provides a compositional synchronization mechanism
    for blocking on system calls, asynchronous function completion and
    synchronization primitives.

    Read the {{!basics}basics} for a conceptual overview. See a
    few {{!examples}examples}.

    @canonical Affect.Action *)

(** {1:actions Actions} *)

type +'a t
(** The type for actions whose invocations synchronize with values
    of type ['a] or an exception. *)

val never : 'a t
(** [never] is an action whose invocations are never enabled, they block
    forever and never synchronize. *)

val always : 'a -> 'a t
(** [always v] is an action whose invocations are always enabled with
    [v], they never block and immediately synchronize with [v]. *)

val choose : 'a t list -> 'a t
(** [choose actions] is a non-deterministic choice between [actions]. When
    [choose actions] is invoked all [actions] are invoked and only the
    first one to be enabled synchronizes. The remaining invocations
    never synchronize regardless of whether they are enabled or not –
    however depending on their nature, their invocation may have had
    an effect on the world for example if they had side-effecting
    {!guard}.

    {b Warning.} For now if multiple actions are enabled at the same
    time [choose] is unfair and left-leaning. However this should {b not}
    be relied on, it may change in the future. *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** [map f action] is an action whose invocations invoke [action] and
    applies [f] to their synchronization result. [f] is guaranteed to
    be called only if the invocation synchronizes. If [f] raises the
    exception is thrown into the action invocation result.

    {b Warning.} If an action invocation ends up being part of a {!choose} in
    which it does not get selected, the invocation may not, depending on the
    nature of the action, be immediately garbage collected. So if [f] is a
    closure, be wary about what it keeps a reference on – if anything.

    {b Warning.} There is no control on the scope in which [f] ends up being
    executed. In elaborate scenarios it may even happen outside the
    scheduler's domains. So it's not wise to perform OCaml effects in them,
    this includes making asynchronous function calls or synchronizing
    actions. *)

val guard : (unit -> 'a t) -> 'a t
(** [guard f] is an action that when invoked invokes [f ()] and then
    invokes the returned action.

    If [f] performs effects on the world these persist even if the resulting
    action does not eventually synchronize.

    This is for example useful for timeouts which need to start counting at the
    time of invocation. See an {{!wait_readable_duration}example}.

    {b Warning.} [f] should not raise exceptions. For now if it does
    it is thrown into the {!invoke} before any form of synchronization
    occurs. This may be changed into a panic in the future. One of the
    problems is that other guards may already have been invoked but the
    action they returned will never get a chance to have gone through the
    synchronization protocol, which could break expected variants. *)

(* If we add this one day, perhaps we could frame it as an undoable guard.
val guard_with_nack : (unit t -> 'a t) -> 'a t
val undoable_guard : (unit t -> 'a t) -> 'a t
val on_discard : (unit -> unit) -> 'a t -> 'a t
(** [guard_with_nack f] is like {!guard} except it also receives
   an event that can be synchronized on in a concurrent activity in [f] to
   receive the information that the action was not chosen for
   synchronization. *)
*)

(** {2:derived Derived combinators} *)

val either : 'a t -> 'b t -> ('a, 'b) Either.t t
(** [either a0 a1] is [choose [map Either.left a0; map Either.right a1]]. *)

(** {1:invoking Invoking} *)

val invoke : 'a t -> 'a
(** [invoke action] invokes [action] and blocks until the invocation
    synchronizes. *)

val invoke_poll : 'a t -> 'a option
    [@@ocaml.deprecated "May be removed in the future."]
(** [invoke_poll action] invokes [action] and returns [None] if the invocation
    cannot synchronize without blocking in which case the invocation is
    discarded and never synchronized.

    {b Warning.} This function may be removed in a future version of the
    library. Polling repeatedly is generally not recommended, it may lead to
    higher computational costs. Besides some invocations will always return
    [None] on polling. *)

(** {1:unblockers Unblockers} *)

type unblocker
(** The type for action unblockers.

    These values are provided by libraries defining actions whose invocations
    rely on external factors to synchronize. Values of this type should just
    be given to the invocation of {!Fun.Async.main} so that the scheduler
    can unblock these action invocations. See for example
    {!Affect_unix.Unix.unblocker}. *)

(** {1:private Private} *)

(** Private definitions (unstable). *)
module Private : sig

  (** Action module with private functionality (unstable).

    These definitions are subject to change even between minor versions of
    the library. *)
  module Action : sig
    type nonrec 'a t = 'a t
    (** See {!Affect.Action.t}. *)

    type 'a primitive

    val never : 'a t
    (** See {!Affect.Action.never}. *)

    val always : 'a -> 'a t
    (** See {!Affect.Action.always}. *)

    val choose : 'a t list -> 'a t
    (** See {!Affect.Action.choose}. *)

    val map : ('a -> 'b) -> 'a t -> 'b t
    (** See {!Affect.Action.map}. *)

    val guard : (unit -> 'a t) -> 'a t
    (** See {!Affect.Action.guard}. *)

    val either : 'a t -> 'b t -> ('a, 'b) Either.t t
    (** See {!Affect.Action.either}. *)

    val invoke : 'a t -> 'a
    (** See {!Affect.Action.invoke}. *)

    val invoke_poll : 'a t -> 'a option
    [@@ocaml.deprecated "May be removed in the future."]
    (** See {!Affect.Action.invoke_poll}. *)

    type nonrec unblocker = unblocker
    (** See {!Affect.Action.unblocker}. *)

    (** {1:private Private} *)

    (** Action results.

        An action result is the value returned by an action when it
        synchronizes. *)
    module Result : sig

      (** {1:results Results} *)

      type 'a t =
      | Value of 'a
      | Exn of exn * Printexc.raw_backtrace (** *)
      (** The type for action results.  *)

      val none : 'a t
      (** [none] is a stub value. Except for initializing data structures, it
          should not be used. If {!return}ed or {!deep_continue}d it
          raises [Assert_failure]. *)

      val of_fun_run : (unit -> 'a) -> 'a t
      (** [of_fun_run f] executes [f ()] and turns it into a result. *)

      (** {1:continuing Continuing} *)

      val return : 'a t -> 'a
      (** [return r] returns or raises the result at the call point. *)

      val deep_continue : ('a, 'b) Effect.Deep.continuation -> 'a t -> 'b
      (** [deep_continue k r] continues [k] with the result [r]. *)

      val deep_continue_thunk :
        ('a, 'b) Effect.Deep.continuation -> 'a t -> unit -> 'b
      (** [deep_continue_thunk k r] is [fun () -> deep_continue k r]. *)

      val some_deep_continue_thunk :
        ('a option, 'b) Effect.Deep.continuation -> 'a t -> unit -> 'b
        (** [some_deep_continue_thunk] is like {!deep_continue_thunk}
            but it wraps a [Value s] into [Value (Some v)]. *)
    end

    (** Action and action invocation metadata.

        Convenience for more informative tracing. *)
    module Meta : sig

      (** {1:meta Metadata} *)

      (** Metadata keys. *)
      module Key : sig
        type 'a t
        (** The type for custom metadata keys. *)

        val make : ?pp_value:(Format.formatter -> 'a -> unit) -> unit -> 'a t
        (** [make ~pp_value ()] is a new key using [pp] to format its values
            (defaults formats ["<value>"]. *)

        val id : 'a t -> 'a Type.Id.t
        (** [id] is the identifier of the key. *)

        val pp_value : 'a t -> Format.formatter -> 'a -> unit
        (** [pp_value key] is [key]'s value formatter. *)
      end

      type name = string
      (** The type for action names. *)

      type binding = Binding : 'a Key.t * 'a -> binding (** *)
      (** The type for metadata bindings. *)

      type t
      (** The type for metadata. *)

      val make : ?bindings:binding list -> name:name -> unit -> t
      (** [make ~name ~meta ()] is metadata for an action named [name] with
          additional metadata meta. *)

      val none : t
      (** [none] is metadata for when there is none. *)

      val name : t -> name
      (** [name meta] is the name of the primitive operation. *)

      val bindings : t -> binding list
      (** [bindings meta] is [meta]'s list of bindings. *)

      (** {1:preds Predicates} *)

      val is_none : t -> bool
      (** [is_none m] is [true] if and only if [m] is {!none}. *)

      (** {1:fmt Formatting} *)

      val pp : Format.formatter -> t -> unit
      (** [pp] formats metadata. *)
    end

    (** Action invocations. *)
    module Invocation : sig

      (** {1:invocations Invocations} *)

      type 'a action := 'a t

      type 'a t
      (** The type for action invocations. *)

      val make :
        ?unblock:'a primitive -> 'a action ->
        (exn * Printexc.raw_backtrace, 'a t) Either.t
      (** [make action] is an invocation for [action] it is:

          {ul
          {- [Right invocation], if everything went well.}
          {- [Left (exn, bt)] if one of the action's {!Action.guard}s
             raised. For now we advise to throw it into the continuation
             of {!Invoke} or {!Invoke_poll}.}}

          [unblock] if provided is added as a choice to [action]
          but is not reported in the {!meta} of the resulting invocation.
          That all it does. For the rest it just behaves as if you had given
          [Action.choose [unblock; action]]. *)

      val meta : 'a t -> Meta.t
      (** [meta invocation] is the metadata derived from the action given at
          {!make}. *)

      (** {1:synchronizing Synchronizing} *)

      val do_poll :
        continue:('a Result.t -> unit -> unit) -> 'a t ->
        (unit -> unit) option
      (** [do_poll ~continue invocation] must be called on an {!Invoke} or
          {!Invoke_poll} effect with [continue] a function that invokes the
          effect's continuation. The result is:
          {ul
          {- [Some k], if the synchronization of [action] succeeds without
             blocking. [k] is [continue] partially applied with the
             invocation's result. It can be directly invoked or scheduled as
             being ready for execution.}
          {- [None] if the synchronization of [action] would block. In this
             case [continue] is left untouched. If you are handling
             {!Invoke_poll} you should continue with [None] and if you are
             handling {!Invoke} you should move on to call {!do_block}}} *)

      val do_block :
        unblock:('a Result.t -> unit) -> 'a t -> unit
      (** [do_block ~unblock invocation] must be called on an {!Invoke} after –
          and {b only} after – a call to {!do_poll} on [invocation]
          returned [None].

          [unblock r] is called with the invocation result [r] when
          [invocation] eventually synchronizes. This call should just
          {b schedule} the continuation of {!Invoke} with [r] in your
          scheduler {b It must not directly invoke it}.

          The call to [unblock] may happen during the [do_block] call
          already, or only later, or never. Given the latter it may
          also never be gc'd or only much later so so think carefully about what
          you hold on to in the closure. Another dimension is that it may
          end up being called in an arbitrary context so if you lookup
          domain local storage prepare for it being absent or present
          but not the one you expect! *)
    end

    (** Blocked action invocations.

        If an action invocation fails to synchronize during
        the {{!Invocation.do_poll}polling phase}. A blocked invocation
        value is created by the {{!Invocation.do_block}blocking phase}
        for the invocation. It holds:

        {ul
        {- The [unblock] function from the scheduler which when called
           schedules the continuation of the {!Action.invoke} blocked on
           the invocation with the invocation result.}
        {- An action invocation state to track its synchronisation
           {{!Blocked.type-state}state}. This state moves to {!Blocked.Synced}
           when the [unblock] function gets called with a result.}}

        If an action invocation is an {!Action.choose}, this blocked invocation
        value is notably given to all the {!Primitive.block} functions of the
        primitive invocations that are part of the choice and competing
        to win the synchronization. The blocked value state is used by
        these {!Primitive.block} functions to ensure that they do not try to
        synchronize the action if it was already synchronized by another
        invocation (or an external entity if the blocked value was published
        for a rendezvous synchronization like in {!Affect.Port}s). *)
    module Blocked : sig

      (** {1:blocked Blocked invocations} *)

      type 'a t
      (** The type for a blocked action invocation.

          This encapsulate an invocation synchronization {{!type-state}state}
          and an {{!tricky_unblock}unblock function} to schedule a
          continuation of the blocked {!Action.invoke} with the result of the
          action invocation. *)

      val none : unit -> 'a t
      (** [none ()] is a blocked invocation for when there is none.
          Trying to unblock it raises [Assert_failure]. *)

      (** {1:unblocking Unblocking operations} *)

      val tricky_unblock : 'a t -> 'a Result.t -> unit
      (** [tricky_unblock b r] calls the unblocking function of [b] with [r].

          {b WARNING.} This function must only be called once and only if you
          managed to bring the state of [b] to {!Synced} yourself. Rather use
          {!synced_unblock} and {!synced_unblock_is_ours} for a safer version
          of this. *)

      val synced_unblock : candidate:'a Result.t -> 'a t -> unit
      (** [synced_unblock ~candidate b] makes sure [b] is synchronized.
          After the function returns it is guaranteed that:
          {ol
          {- The {{!get_state}state} of [b] is {!Synced}}
          {- The {{!tricky_unblock}unblocking function} of [b] has been called,
             possibly using [candidate] (if we moved the state of [b]
             with an {!exchange_waiting_to_synced}).}} *)

      val synced_unblock_is_ours : candidate:'a Result.t -> 'a t -> bool
      (** [synced_unblocked_is_ours ~candidate b] is like {!synced_unblock} but
          returns [true] if and only if [candidate] was used to call the
          unblocking function (we moved the state of [b] with
          {!exchange_waiting_to_synced}) and [false] otherwise.

          This is used when a synchronization has a rendez-vous semantics.
          If it returns [false] it means our [candidate] was
          not responsible for the synchronization and we should find another
          blocked value to provide it to. *)

      (** {1:state State}

          The state operations are atomic. *)

      type state =
      | Waiting (** The invocation is available for synchronization. *)
      | Claimed (** The owner of the invocation is attempting to synchronize it
                    in the [block] phase. *)
      | Synced (** The invocation has been synchronized and has returned. *)
      (** The type for blocked action invocation synchronization states. *)

      val get_state : 'a t -> state
      (** [get_state b] is the state of the blocked invocation. *)

      val set_state : 'a t -> state -> unit
      (** [set_state b st] sets the state of the blocked operation.
          See also the {!state_authorized}. *)

      val exchange_waiting_to_synced : 'a t -> state
      (** [exchange_waiting_to_synced s] tries to atomically move [s]
          from [Waiting] to [Synced] and returns the previous state value:
          {ul
          {- If [Waiting] is returned the state change succeeded. The
             synchronization is ours, we must unblock the suspended
             {!Action.invoke} by calling {!tricky_unblock}.}
          {- If [Claimed] is returned, the action is being synchronized by
             the owner of the invocation, it may fail to do so though so
             try again with a backoff.}
          {- If [Synced] is returned the action already synchronized. Just move
             on and unreference the blocked value.}}

          See also {!synced_unblock} and {!synced_unblock_is_ours} which
          are slightly higher level: they combine the retries on {!Claimed}
          returns, the call to the unblocking function on {!Waiting} returns
          and the nop on {!Synced} returns.

          {b Note.} This transition is the only one you are allowed to
          perform on blocked values that are not yours that you find in data
          structures that hold blocked values. See also the
          {!state_authorized}. *)

      val exchange_waiting_to_claimed : 'a t -> state
      (** [exchange_waiting_to_claim s] tries to atomically move [s] from
          [Waiting] to [Claimed] and returns the previous value:

          {ul
          {- If [Waiting] is returned the state change succeeded. The action
             is yours to perform a synchronization protocol on other blocked
             values.}
          {- If [Claimed] is returned this should normally not happen
             since we are responsible to move it.}
          {- If [Synced] is returned the action already synchronized
             concurrently. Just move on and unreference the blocked value.}}

          {b Warning.} This transition must only be done in a
          {!Primitive.block} function and on the blocked value that is handed
          to you. It gives you a "lock" on the value while you try to
          synchronize another blocked value
          with {!exchange_waiting_to_synced} in order to perform
          a rendezvous synchronization with another invocation (like in
          {!Affect.Port}s). If you fail to achieve your goal you must
          move the state back to {!Waiting} with {!set_state}.
          See also the {!state_authorized}. *)

      val is_synced : 'a t -> bool
      (** [input_is_synced b] is [true] if and only if [b]'s action state
          is {!Synced}. *)

      val is_not_synced : 'a t -> bool
      (** [not_synced b] is [true] if and only if [b]'s action state is
          not {!Synced}. *)

      val is_not : 'a t -> 'b t -> bool
      (** [is_not b0 b1] is [true] if and only if [b0]'s state is not physically
          equal to [b1]'s. This means that [b0] and [b1] do not
          represent the same action invocation. This is usually needed
          to avoid having {!Action.choose} having a rendezvous between
          two of its primitive invocations. For example if both a
          {!Port.offer'} and {!Port.take'} on the same port are in the
          same choice. *)

      (** {2:state_authorized Authorized state transitions}

          {b Note.} These descriptions may be clearer after reading
          the explanations of
          {{:https://doi.org/10.1145/1596550.1596588}this paper}
          and looking at the state diagrams on p.5. This
          {{:https://wingolog.org/archives/2017/06/29/a-new-concurrent-ml}blog
          post} could also help making sense of it.

          In a {!Primitive.block} function, before definitvely
          blocking by storing the [blocked] value in data structures for
          later synchronization, the following state transitions can be
          performed on the [blocked] value we are handed with:

          {ul
          {- [Waiting] to [Claimed] with {!exchange_waiting_to_claimed}.
             This allows the primitive to get a "lock" on the action invocation
             synchronization while we try to synchronize another blocked
             value. For example in the case of {!Affect.Port}, if we try
             to [Port.take] a value we need to find a matching
             [Port.offer] blocked value and try to move its state from
             [Waiting] to [Synced] to finalize the rendezvous.}
          {- [Claimed] to [Synced]. There is no competition on this transition,
             because in the [Claimed] state we have the "lock" on the state.
             We can simply {{!set_state}set} the state to perform it.
             In our example we do this transition when we managed to
             synchronize the chosen [Port.offer] blocked value. This means
             that the rendezvous occured and we succesfully finished the
             [Port.take] with the offer we chose to synchronize with.}
          {- [Claimed] to [Waiting]. There is no competition on this transition,
             because in the [Claimed] state we have the "lock" on the state.
             We can simply {{!set_state}set} the state to perform it.
             In our example we do this if we did not manage to synchronize the
             chosen [Port.offer] blocked value. The rendezvous did not occur
             and we need to try to find another offer.}
          {- [Waiting] to [Synced] with {!exchange_waiting_to_synced}.
             It possible to bypass the [Claimed] state
             if we don't need to synchronize with another action (e.g. if we
             are just interacting with the operating system).
             But we need to compete to perform this transition because
             we may be part of an action choice where other primitives
             are trying to synchronize the blocked value.}}

          {b Note.} Once in the [Claimed] state there is no competition
          to change the state because each primitive is tried one after the
          other in {!Action.Invocation.do_block}. However there is a
          competition to bring it to that state because other primitive
          actions that are part of the choice may have blocked and thus
          have published the [blocked] value for other concurrent activities
          to try to synchronize it (without going through [Claimed] since
          they are not allowed to do so).

          The state transition that we are allowed to perform on
          any blocked value we find in {!Primitive.poll}, {!Primitive.block} or
          in primitive action invocation
          {{!Unblocker.Domain_local.type-unblock}unblocker functions}
          is {b only}:

          {ul
          {- [Waiting] to [Synced]. If that succeeds the synchronization is
             ours and we must unblock the value. It is recommended
             to use {!synced_unblock} or {!synced_unblock_is_ours} for
             performing these transitions.}}
      *)

      (** {1:patterns Higher-level blocking patterns} *)

      (** {2:with_value Blocked with a value} *)

      (** Blocked with a value.

          This is the same API as {!Action.Blocked} but for when
          you already know (and usually store) the value to synchronize
          with. Typically used for waiting actions tag synchronize with
          a user provided tag. For example {!Fun.Async.wait_cancelled},
          {!Port.offer'}, etc. *)
      module Value : sig
        type 'a blocked := 'a t

        type t = V : 'a Result.t * 'a blocked -> t
        (** The value to resume with and the blocked invocation. *)

        val make : 'a -> 'a blocked -> t
        (** [make v b] is [V (Action.Result.Value v, b)]. *)

        val exchange_waiting_to_synced : t -> state
        (** [exchange_waiting_to_synced (V (_, b))] is
            [exchange_waiting_to_synced b]. *)

        val exchange_waiting_to_claimed : t -> state
        (** [exchange_waiting_to_claimed (V (_, b))] is
            [exchange_waiting_to_claimed b]. *)

        val set_state : t -> state -> unit
        (** [set_state (V (_, b)] is [set_state b s]. *)

        val is_synced : t -> bool
        (** [is_synced (V (_, b)] is [is_synced b]. *)

        val is_not_synced : t -> bool
        (** [is_not_synced (V (_, b)] is [is_not_synced b]. *)

        val is_not : t -> 'b blocked -> bool
        (** [is_not (V (_, b0))] b1 is [is_not b0 b1]. *)

        val tricky_unblock : t -> unit
        (** [tricky_unblock (V (v, b)] is [tricky_unblock b v]. *)

        val synced_unblock : t -> unit
        (** [synced_unblock (V (v, b))] is
            [synced_unblock ~candidate:v b]. *)

        val synced_unblock_is_ours : t -> bool
        (** [synced_unblock_is_ours (V (v, b))] is
            [synced_unblock_is_ours ~candidate:v b]. *)
      end
    end

    (** Primitive actions.

        After an action invocation as been created it is first
        {{!Invocation.do_poll}polled} and if that doesn't yield a
        synchronization, it is {{!Invocation.do_block}blocked}.
        A primitive action needs to define what happens during
        these two phases by providing corresponding functions.

        {b Debug tip.} Heisen synchronization hangs usually happen during the
        block phase. Unconditionally returning [None] in the poll phase
        helps with reproducing the hang more often. *)
    module Primitive : sig

      (** {1:polling Polling} *)

      type 'a poll =
        continue:('a Result.t -> unit -> unit) -> (unit -> unit) option
      (** The type for optimisitically polling a primitive action for
          sychronization.

          If the polling phase is able to provide a result [r] for the
          invocation it should apply [continue r] and return the resulting
          thunk. If it fails it {b must} return [None] and not touch or
          store [continue]. Always returning [None] is a legitimate
          implementation of the [poll] phase, see {!poll_is_none}. For an
          invocation, this function is always guaranteed to be called before
          {!block} does.

          {b Warning.} The function must not raise. If it's meaningful
          for your action to do so you should [continue] with a
          {!Result.Exn} value. If an unexpected exception occurs the current
          implementation throws it in [continue] but do not rely on this as it
          may change in the future. *)

      val poll_is_none : 'a poll
      (** [poll_is_none] is a polling function that always returns [None]. *)

      (** {1:blocking Blocking} *)

      type 'a block = blocked:'a Blocked.t -> unit
      (** The type for blocking on a primitive action.

          This function is only invoked if the {!type-poll} phase returned
          [None]. It assumes that the entity blocked on the invocation has
          been suspended and that it can be resumed by
          {{!Action.Blocked.unblocking}unblocking} the blocked value.

          The function {b must} try to synchronize again like it did in
          the {!type-poll} phase before storing the [blocked] somewhere until
          it can unblocked by another entity. If while doing so you
          discover that the state of [blocked] is already {!Blocked.Synced},
          you can just throw the value away and do nothing. In fact
          if you store [blocked] in data structures and never get to
          synchronize it you should make sure to periodically check
          if it reach that state and garbage collect it (usually when you are
          adding or removing blocked values from your data structures).

          There are two reasons why [blocked] value may synchronize without
          you being in charge:

          {ul
          {- Your primitive invocation may be part of a {!Action.choose}
             invocation. The {{!Blocked.val-get_state}state} of the
             [blocked] value represents that choice which is shared among all
             invocations of the choice. If another invocation synchronizes the
             choice, the state becomes {!Blocked.Synced} and you must not
             synchronize the blocked value (that would invoke the
             {!Action.invoke} continuation twice and break the semantics
             of {!Action.choose}).}
          {- If you are trying to synchronize with another action for
             a rendezvous like {!Port}s do. As soon as
             [blocked] is published for others to see, other action
             invocations may be trying to synchronize it concurrently.}}

          {b Warning.} The function must not raise. If it is meaningful for
          your action to do so you should unblock [blocked] with a
          {!Result.Exn} value. If an unexpected exception occurs the
          current implementation unblocks [blocked] with it but do not
          rely on this it may change in the future. *)

      (** {1:primitives Primitive actions} *)

      val make : meta:Meta.t -> poll:'a poll -> block:'a block -> 'a t
      (** [make ~meta ~poll ~block] is a primitive action whose invocation is
          first polled with [poll] and if that does not yield a synchronization
          blocked with [block]. *)

      (** {1:bare_primitivies Bare primitives} *)

      type 'a t = 'a primitive
      (** The type for bare primitive actions. *)

      val make_bare : meta:Meta.t -> poll:'a poll -> block:'a block -> 'a t
      (** [make_bare] is like {!make} but create a bare primitive action
          for use with {!Invocation.make}. *)
    end

    (** Action invocation unblockers.

        Action invocation unblockers are provided by libraries defining actions
        whose invocations rely on external factors to synchronize. See for
        example {!Affect_unix.Unix.unblocker}.

        {b Note.} For the time being, due to the blocking behaviour of
        {!Unblocker.Domain_local.val-unblock} when invoked with [block:true],
        unblockers are not naturally composable. The best that libraries
        can do is to cooperate to try to give users a handle on the
        constituents of the unblocking mecanisms so that composite unblockers
        can be easily devised (see for example the components that make
        up the {!Affect_unix.Unix.unblocker}, though those are still a bit
        untidy and not exposed for the time being). *)
    module Unblocker : sig

      (** {1:local Domain local unblocker} *)

      (** Domain local unblockers. *)
      module Domain_local : sig

        (** {1:local Domain local unblockers} *)

        type unblock = block:bool -> bool
        (** The type for action invocation unblocking functions.

            A call to the function must unblock the action invocations that can
            synchronize. The [block] argument is such that:

            {ul
            {- If [false] the function must not block if there is nothing ready
               to unblock or nothing to unblock at all.}
            {- If [true], the function must block until either an action
               invocation does unblock or until {!type-set_block_bypass} is
               called. The scheduler ensure that a single local unblocker
               is invoked with [~block:true] at the same time.}}
            In all cases the function must return:
            {ul
            {- [true] if and only if it did sucessfully unblock (in the
               {!Private.Action.Blocked.synced_unblock_is_ours} sense) at
               least one action invocation. False positive are ok but do not
               always return [true] as it may degenerate the scheduler into
               a busy waiting loop when there is no work.}
            {- [false] if no action invocation was unblocked by the call.}}

          {b Warning.} The function must not raise. If it does, the entity
          invoking the function should panic and not try to recover. If you
          have errors that happen because of user errors e.g. operations
          on a closed fd, you should either try discontinue the blocked action
          invocations and/or trap the exception so that it doesn't go
          unoticed. Do not trap asynchronous exceptions though. *)

        type set_block_bypass = unit -> unit
        (** The type for indicating that the next or current call to
            {!type-unblock} with [~block:true], should not block.

            If the call happens while [unblock ~block:true] is already
            naturally returning, the [unblock] call can clear the bypass or
            not, it's harmless. One way of thinking about this mecanism is to
            implement it using a self-pipe trick to unblock a blocking
            [select(2)]; calling [set_block_bypass] makes the pipe
            readable until cleared (which is the responsability of
            [unblock]). *)

        type t
        (** The type for a per domain unblocker. It is guaranteed
            to be called only by the main thread of the domain it
            was created for, see {!Action.Unblocker.val-domain_local}. *)

        val make :
          unblock:unblock -> set_block_bypass:set_block_bypass -> unit -> t
        (** [make] is a local unblocker with given properties. See accessors for
            details on how these functions end up being called by the
            scheduler. *)

        val unblock : t -> block:bool -> bool
        (** [unblock u ~block] tries to unblock primitive invocations. This is
            called by scheduler domains with [block:false] when they did not
            find work. If that doesn't unblock work they either go to sleep
            or if they are the last domain call the function again with
            [block:true]. See also {!type-unblock} for details. *)

        val set_block_bypass : t -> unit
        (** [set_block_bypass u] indicates that the current or next blocking
            call to [unblock] should not block. This is used by the scheduler
            to unblock when new work is available and a domain is blocked
            with [unblock ~block:true] or on its way to do so. This is needed
            to coordinate schedulers with external entities which are unknown
            to the scheduler. See also
            {!type-set_block_bypass} for details. *)

        val none : t
        (** [none] is an unblocker for when there is none. Trying to invoke its
            functions raise assertion failures. *)

        (** {1:nothing Unblock nothing} *)

        val nothing : unit -> t
        (** [nothing] is a local unblocker that unblocks nothing. It blocks by
            sleeping on a condition variable. It is used by the
            {!Unblocker.val-nothing} unblocker. *)
      end

      (** {1:unblocker Unblockers} *)

      type index = int
      (** The type for domain scheduler indexes. Index [0] is the only index
          that is guaranteed to exist, it corresponds to the scheduler's
          main domain.

          {b Note.} These indices are allocated by the scheduler,
          they are a separate concept from {!Domain.self_index} indices. *)

      type init = domain_count:int -> unit
      (** The type for initializing an unblocker. See {!val-init}. *)

      type domain_local = index -> Domain_local.t
      (** The type for getting a domain local unblocker for a given domain
          index. See {!val-domain_local}. *)

      type domain_install = Domain_local.t -> unit
      (** The type for installing a local unblocker on a domain.
          See {!val-domain_install}.*)

      type domain_uninstall = Domain_local.t -> unit
      (** The type for uninstalling a local unblocker on a domain.
          See {!val-domain_uninstall}. *)

      type deinit = unit -> unit
      (** The type for deinitializating an unblocker. See {!val-deinit}. *)

      type t = unblocker
      (** The type for unblockers. *)

      val make :
        init:init -> domain_local:domain_local ->
        domain_install:domain_install -> domain_uninstall:domain_uninstall ->
        deinit:deinit -> unit -> t
      (** [make] is an unblocker with given properties. See accessors for
          details on how these functions end up being called by the
          scheduler. *)

      val init : t -> domain_count:int -> unit
      (** [init u ~domain_count] is called {b once} by the scheduler main
          domain with the total number of domains that are going to exist in
          the scheduler (they may or may not have been already spawned). This
          call occurs before any call to {!val-domain_local},
          {!val-domain_install}, {!val-domain_uninstall} or {!val-deinit}. *)

      val domain_local : t -> index -> Domain_local.t
      (** [domain_local u i] is called {b once} by the scheduler for each domain
          [i] in the scheduler to get the local unblocker for domain [i]. This
          call is made after {!val-init} has been called.
          The domain that executes the function [local u i] is unspecified. *)

      val domain_install : t -> Domain_local.t -> unit
      (** [domain_install u l] is called {b once} by the domain [i] of
          the scheduler with the result of [local u i]. *)

      val domain_uninstall : t -> Domain_local.t -> unit
      (** [domain_uninstall u l] is called {b once} by the domain [i] of the
          scheduler after [domain_install u l] has been called just before the
          local scheduler of the domain finally stops. It is only called if
          a corresponding {!val-domain_install} has been made. *)

      val deinit : t -> unit -> unit
      (** [deinit u ()] is called {b once} by the scheduler main domain, after
          all the scheduler domains have been joined. It's only called if
          {!val-init} was ever called on [u]. *)

      (** {1:nothing Unblock nothing} *)

      val nothing : unit -> t
      (** [nothing] is an unblocker that unblocks nothing. It is
          the default of {!Fun.Async.main}. *)
    end

    (** {2:effects Effects}

        {b Note.} You should never handle these effects in
        the scope of a function given to {!Affect.Fun.Async.main}.
        These effects
        are exposed to allow actions to be used by other schedulers.
        See an example
        {{:https://github.com/dbuenzli/affect/blob/master/test/test_thread.ml}
        here}. *)

    type 'a Effect.t +=
    | Invoke : 'a t -> 'a Effect.t (** Performed on {!Action.invoke}. *)
    | Invoke_poll : 'a t -> 'a option Effect.t
    (** Performed on {!Action.invoke_poll}. *)
    (** Effects for handling action invocations.
        {ul
        {- On {!Invoke} you must create an invocation for the action with
           {!Invocation.make} and follow with the synchronization protocol.
           See {!Invocation.synchronizing}.}
        {- On {!Invoke_poll} you must create an invocation with
           {!Invocation.make} and just try to poll the invocation. The code
           is always the same so you can just use {!do_invoke_poll}
           to handle this.}} *)

    val do_invoke_poll :
      ?unblock:'a Primitive.t -> 'a t ->
      ('a option, unit) Effect.Deep.continuation -> unit
    (** [do_invoke_poll action k] does everything you need to do to handle
        the {!Invoke_poll} effect with a continuation [k]. It can implemented
        using {!Invocation.make} and {!Invocation.do_poll} but it's always
        the same code, so here you have. For the argument [unblock] see
        {!Invocation.make}. *)
  end
end

(** {1:basics Basics}

    Action values are like function values, they represent an abstract
    operation that can be invoked to eventually return a value or an
    exception. When an action invocation returns we say that it
    {e synchronizes}. The series of steps action invocations
    follow to return values is what makes them different from functions
    and what allows them to abstract synchronization protocols.

    Actions are invoked with the {!Action.invoke} function:
    {[
    val Action.invoke : 'a Action.t -> 'a
    ]}
    This function creates an invocation for the action and blocks the caller
    until the action synchronizes.

    Before being able to synchronize, an action invocation needs be
    {e enabled}, which means ready to return a value. The condition for the
    action invocation to be enabled is determined by the nature of the action
    and can depend on:

    {ul
    {- External factors. For example an operating system file descriptor
       being ready for reading.}
    {- State in the program. For example a value having been stored in
       a {{!Cell.Once}set once} cell.}
    {- The existence of another enabled action invocation.
       For example for a {!Port.take'} action invocation to be enabled,
       an enabled {!Port.offer'} action invocation must exist
      (and vice-versa).}}

    Once enabled, an action invocation may eventually synchronize but
    it also may never do so for one of these reasons:

    {ul
    {- Something happened in the system and the invocation is no longer
       enabled at the time of synchronization. For example
       a {!Port.take'} invocation failed to simultanously synchronize with
       a matching {!Port.offer'} invocation because another enabled
       {!Port.take'} invocation took the offer and no other enabled
       {!Port.offer'} invocation on the port ever becomes available.
       In that case we are blocked forever on the invocation.}
    {- The invocation is still enabled but it is discarded because it is
       part of a non-deterministic choice among competing enabled action
       invocations (more on this just below).}}

    The fact that an action invocation may be prevented from synchronizing
    allows to abstract a non-deterministic choice between competing action
    invocations with the {!Action.choose} combinator:

    {[
    val Action.choose : 'a Action.t list -> 'a Action.t
    ]}

    This combinator takes a list of actions and returns an action which
    when invoked invokes each action in the list. It then selects the first
    one to become enabled, synchronizes it and returns with its value.
    All the other invocations are prevented from synchronizing (even
    though depending on their nature, their invocation may have had an effect
    on the world).

    For example the following derived [propose'] action can be invoked on a
    {{!Affect.Port}port} [p]:

    {[
    let propose' p v =
      let ours = Action.map (fun () -> v) (Port.offer' p v ()) in
      let theirs = Port.take' p in
      Action.choose [ours; theirs]
    ]}

    The [propose'] action either offers or receives a value on a port [p] and
    synchronizes with either value. If a port [p] is only known to two
    functions that both invoke the action [propose'] with their own value it
    guarantees they will both synchronize with a consensus on one of the
    proposed values
    (before you try, note that a three-way synchronous consensus cannot
     be achieved with the set of combinators provided by
    affect, see
     {{:https://doi.org/10.1017/S0956796808006916}this paper}).

    In the example above, the semantics of {!Action.choose} which synchronizes
    a {e single} enabled action invocation and discards the others ensures
    that the choice made in [propose'] between {!Port.take'} and
    {!Port.offer'} on the same port does not self-synchronize.

    Finally, note that invoking the same action more than once
    can lead to different synchronization behaviours over time. The
    next section describes a few common behaviours.
*)

(** {2:synchronization_typology Synchronization typology}

    The synchronization behaviour of invocations of a given
    action can evolve over time. Here are typical behaviours:
    {ul
    {- Invariant action. All invocations synchronize exactly the same
       way on each invocation.

       Examples are timer actions like {!Affect_unix.Mtime.wait_for'}
       or {!Affect_unix.Ptime.wait_until'}.}
    {- Waiter action. All invocations block until a point where they
       all synchronize. After the synchronization point all invocations
       behave like {!always}.

       Examples are {!Affect.Fun.Async.get'},
       {!Affect.Fun.Async.wait_cancelled},
       {!Affect.Cell.Lazy.force'}, {!Affect.Cell.Once.get'}.}
    {- Single shot exclusive action. Exactly one invocation blocks until
       a point where it synchronizes and all other invocations behave like
       {!never}.}
    {- Single shot inclusive action. All invocations block until a point
       where they all synchronize. After the synchronization point all
       invocations behave like {!never}.

       For example a single-shot barrier action.}
    {- Matching action. An invocation blocks until some different action
       invocation occurs and both synchronize simultaneously.

       Examples are {!Port.offer'} and {!Port.take'}.}} *)

(** {1:examples Examples}

    Some examples need to following preamble:

{[
open Affect
open Affect_unix
]}

    {3:fancy_wait_readable Fancy wait readable}

    The fancy action below waits for a file descriptor to become
    readable but gives up after 1min30s or if a [Sys.sigsur1] signal
    gets delivered to the process.

{[
let fancy_wait_readable :
  Unix.file_descr -> [ `Readable | `Timeout | `User1_interrupt ] Action.t
=
fun fd ->
  let readable = Unix.wait_readable fd `Readable in
  let timeout = Mtime.wait_for' Mtime.Span.(1*min + 30*s) `Timeout in
  let signal = Unix.Signal.wait' Sys.sigusr1 `User1_interrupt in
  Action.choose [readable; timeout; signal]
]}

    {3:wait_readable_duration Wait readable duration}

    The action below is like {!Affect_unix.Unix.wait_readable} but it uses
    a {{!Affect_unix.Mtime.type-counter}monotonic counter} to measure the time
    it took for the file descriptor to become readable.

{[
let wait_readable_duration : Unix.file_descr -> Mtime.Span.t Action.t =
fun fd ->
  let setup () =
    let counter = Mtime.counter () in
    let readable = Unix.wait_readable fd counter in
    Action.map Mtime.count readable
  in
  Action.guard setup
]}

    {3:speculation Say no to speculation}

    The action below can be used if you have an asynchronous function
    that returns partial speculated results on cancellation that you
    don't find useful. It gives you the equivalent of a {!Fun.Async.get'}
    action that ignores the results in case the asynchronous function
    was cancelled.
{[
let no_speculation_get : 'a Fun.Async.t -> 'a option Action.t =
fun f ->
  let wait_cancelled = Fun.Async.wait_cancelled f None in
  let get = Action.map Option.some (Fun.Async.get' f) in
  Action.choose [wait_cancelled; get]
]}
*)
