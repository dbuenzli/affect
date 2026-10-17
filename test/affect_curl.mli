(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** cURL transfers. *)

(** {1:setup Setup} *)

val init : ?opts:Curl.Multi.curlMultiOption list -> unit -> unit
(** [init] must be called before any function of this module and after a
    {!Curl.global_init} and in the scope of an affect {!Affect_unix.unblocker}.

    {b Warning.} This function creates a looping asynchronous function call
    to handle cURL transfers. The caller won't return before
    {!deinit} is called. *)

val deinit : unit -> unit
(** [deinit ()] must be called after {!init} and once you no longer
    want to perform transfers. If there are still transfers being performed
    they are {!abort}ed. *)

(** {1:transfers Transfers} *)

val perform : Curl.t -> Curl.curlCode
(** [perform h] cooperatively performs the transfer [h] and waits for it to
    complete.

    @raise Invalid_argument if [h] is already being performed. *)

val perform' : Curl.t -> Curl.curlCode Affect.Action.t
(** [perform' h] is the single shot exclusive action that performs the
    transfer described by [h]. The action invocation is enabled and
    synchronizes with the transfer result code when the transfer is complete.

    @raise Invalid_argument if [h] is already being performed. *)

val abort : Curl.t -> unit
(** [abort h] aborts the transfer [h]. If [h] being {!perfomed'} the the
    action synchronizes with code {Curl.CURLE_ABORTED_BY_CALLBACK}. Otherwise
    it has no effect. *)
