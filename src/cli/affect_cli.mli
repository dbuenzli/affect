(*---------------------------------------------------------------------------
   Copyright (c) 2026 The affect programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Cmdliner support for [affect]ed programs. *)

val parallel_count :
  ?absent:string -> ?opts:string list -> ?env:Cmdliner.Cmd.Env.info -> unit ->
  int option Cmdliner.Term.t
(** [parallel_count ()] is a command line argument to specify the strictly
    positive number of domains to run in the program. The resulting
    option value can be directly used with the [domain_count] argument
    of {!Affect.Fun.Async.main}.

    {ul
    {- [opts] are the options names of the argument. It defaults to
       [["P";"parallel-count"]]}
    {- [absent] describes how the value is determined when the value is [None].
       It defaults to the value of {!Domain.recommended_domain_count}
       (the default of {!Affect.Fun.Async.main}).}
    {- [env] is an environment variable to define the default value.
       It defaults to [PARALLEL_COUNT].}}

    We use the word "parallel" rather than "domain" because the latter
    is OCaml terminology that makes little sense to an end user. *)

val parallel_trace :
  ?opts:string list -> ?env:Cmdliner.Cmd.Env.info -> unit ->
  bool Cmdliner.Term.t
(** [parallel_trace ()] is a command line argument to indicate
    to enable affect tracing.
    {ul
    {- [opts] are the option names of the argument. It defaults to
       [["T";"parallel-trace"]]}
    {- [env] is a boolean environment variable to define the default value.
       It defaults to [PARALLEL_TRACE].}} *)

val set_parallel_trace :
  ?opts:string list -> ?env:Cmdliner.Cmd.Env.info ->
  ?reporter:Affect.Fun.Async.Trace.reporter -> unit -> unit Cmdliner.Term.t
(** [set_parallel_trace] is like {!parallel_trace} but directly sets the
    reporter to [reporter] when enabled (defaults to
    {!Affect.Fun.Async.Trace.stderr_reporter}). *)
