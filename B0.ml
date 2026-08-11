open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let unix = B0_ocaml.libname "unix"
let threads = B0_ocaml.libname "threads"
let compiler_libs_toplevel = B0_ocaml.libname "compiler-libs.toplevel"

let affect = B0_ocaml.libname "affect"
let affect_unix = B0_ocaml.libname "affect.unix"
let affect_cli = B0_ocaml.libname "affect.cli"
let affect_tmp = B0_ocaml.libname "affect.tmp"
let cmdliner = B0_ocaml.libname "cmdliner"

(* Libraries *)

let affect_lib =
  let srcs = [ `Dir ~/"src"; `X ~/"src/affect_top_init.ml" ] in
  (* Threads is only needed for [Thread.{self,id}] *)
  let requires = [threads] in
  B0_ocaml.lib affect ~srcs ~requires

let affect_unix_lib =
  let srcs = [ `Dir ~/"src/unix"; `X ~/"src/unix/affect_unix_top_init.ml" ] in
  let exports = [affect; unix] in
  B0_ocaml.lib affect_unix ~srcs ~requires:[unix; affect] ~exports

let affect_cli_lib =
  let srcs = [ `Dir ~/"src/cli" ] in
  B0_ocaml.lib affect_cli ~srcs ~requires:[cmdliner; affect] ~exports:[affect]

let affect_tmp_lib =
  let srcs = [ `Dir ~/"src/tmp"; `X ~/"src/tmp/affect_tmp_top_init.ml" ] in
  let exports = [affect; affect_unix] in
  B0_ocaml.lib affect_tmp ~srcs ~requires:[affect_unix; affect] ~exports

(* Tests *)

let test_common_ml = `File ~/"test/test_common.ml"

(* This may be needed when we move back net to more.
let net_mli = `File ~/"test/net.mli"
let net_ml = `File ~/"test/net.ml" *)

let test
    ?(with_common = false) ?(with_net = false) ?(srcs = []) ?(requires = [])
  =
  let srcs = if with_common then test_common_ml :: srcs else srcs in
  (*  let srcs = if with_net then net_mli :: net_ml :: srcs else srcs in *)
  let requires =
    if with_common
    then unix :: b0_std :: cmdliner :: affect :: requires else requires
  in
  let requires =
    if with_net
    then affect_tmp :: affect_unix :: requires else requires
  in
  B0_ocaml.test ~srcs ~requires:(affect_cli :: affect :: requires)

let test_affect = test ~/"test/test_affect.ml" ~with_common:true
let test_stress_affect = test ~/"test/test_stress_affect.ml" ~with_common:true
let test_action = test ~/"test/test_action.ml" ~with_common:true

let test_multi_scheduler =
  test ~/"test/test_multi_scheduler.ml" ~with_common:true

let test_thread =
  let requires = [threads; affect_unix] in
  test ~/"test/test_thread.ml" ~with_common:true ~requires

let test_unix =
  let requires = [threads] in
  test ~/"test/test_unix.ml" ~with_common:true ~with_net:true ~requires

let test_stress_unix_time =
  let requires = [affect_unix] in
  test ~/"test/test_stress_unix_time.ml" ~with_common:true ~requires

let test_port_hang =
  test ~/"test/test_port_hang.ml" ~with_common:true ~run:false

(* Blueprints and sample code *)

let quick_start = test ~/"test/quick_start.ml" ~run:false
let blueprint_minimal =
  test ~/"test/blueprint_minimal.ml" ~requires:[affect] ~run:false

let blueprint_minimal_unix =
  let requires = [affect; affect_unix] in
  test ~/"test/blueprint_minimal_unix.ml" ~requires ~run:false

let blueprint_cli =
  let requires = [affect; cmdliner; affect_cli] in
  test ~/"test/blueprint_cli.ml" ~requires ~run:false

let blueprint_op =
  let requires = [affect; cmdliner; affect_cli] in
  test ~/"test/blueprint_op.ml" ~requires ~run:false

let examples =
  let requires = [affect; affect_unix] in
  test ~/"test/examples.ml" ~requires ~run:false

let cookbook =
  let requires = [affect; affect_unix] in
  test ~/"test/cookbook.ml" ~requires ~run:false

let ping =
  let requires = [affect; affect_unix] in
  test ~/"test/ping.ml" ~requires ~run:false ~with_net:true

(* Benchmarks *)

let mandelbrot =
  let doc = "Mandelbrot set rendering" in
  test ~/"test/mandelbrot.ml" ~doc ~requires:[affect_cli; cmdliner] ~run:false

let hyperfine_scan_domain_count subject ~out =
  let max_domain = Domain.recommended_domain_count () in
  let cmd = Cmd.to_string Cmd.(subject % "-P" % "{domain_count}") in
  Cmd.(tool "hyperfine" % "--export-json" %% path out % "-P" % "domain_count" %%
       int 1 %% int max_domain % cmd)

let bench_mandelbrot =
  let doc = "Benchmark mandelbrot" in
  let units = [mandelbrot] in
  let meta = B0_meta.(empty |> tag bench) in
  B0_unit.of_action' "bench_mandelbrot" ~meta ~doc ~units @@ fun env _ ~args ->
  let* exe = B0_env.unit_exe_file_cmd env mandelbrot in
  let cmd = Cmd.(exe % "-p" %% int 750 % "-m" %% int 250) in
  let cwd = B0_env.scope_dir env in
  let out = Fpath.(cwd / "test" / "bench" / "mandelbrot.json") in
  let bench = hyperfine_scan_domain_count cmd ~out in
  Ok (Os.Exit.execv ~cwd bench)

(* Packs *)

let default =
  let meta =
    B0_meta.empty
    |> ~~ B0_meta.authors ["The affect programmers"]
    |> ~~ B0_meta.maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> ~~ B0_meta.homepage "https://erratique.ch/software/affect"
    |> ~~ B0_meta.online_doc "https://erratique.ch/software/affect/doc"
    |> ~~ B0_meta.licenses ["ISC"]
    |> ~~ B0_meta.repo "git+https://erratique.ch/repos/affect.git"
    |> ~~ B0_meta.issues "https://github.com/dbuenzli/affect/issues"
    |> ~~ B0_meta.description_tags
      ["effects"; "concurrency"; "parallelism"; "fibers"; "org:erratique";
       "cml"; "unix"]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"
          "--with-cmdliner" "%{cmdliner:installed}%"]]|}
    |> ~~ B0_opam.depopts ["cmdliner", ""]
    |> ~~ B0_opam.conflicts [ "cmdliner", {|< "2.0.0"|}]
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "5.5.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.1.0"|}; ]
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"affect" ~meta ~locked:true @@
  B0_unit.list ()
