open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let unix = B0_ocaml.libname "unix"

let affect = B0_ocaml.libname "affect"
let affect_unix = B0_ocaml.libname "affect.unix"

(* Libraries *)

let affect_lib =
  let srcs = [ `Dir ~/"src"; `X ~/"src/affect_top_init.ml" ] in
  B0_ocaml.lib affect ~srcs

let affect_unix_lib =
  let srcs = [ `Dir ~/"src/unix" ] in
  B0_ocaml.lib affect_unix ~srcs ~requires:[unix; affect]

(* Tests *)

let test ?(requires = []) = B0_ocaml.test ~requires:(affect :: requires)

let test_affect =
  let doc = "Fiber tests" in
  test ~/"test/test_fiber.ml" ~doc ~requires:[b0_std; affect]

let test_unix =
  let requires = [b0_std; unix; affect_unix] in
  test ~/"test/test_funix.ml" ~doc:"affect.unix tests" ~requires

let test_busy =
  let requires = [unix; affect_unix] in
  test ~/"test/test_busy.ml" ~doc:"No CPU used!" ~requires ~run:false

let ping =
  let requires = [unix; affect_unix] in
  test ~/"test/ping.ml" ~doc:"Ping-pong test" ~requires ~run:false

let mouse =
  let tsdl = B0_ocaml.libname "tsdl" in
  test ~/"test/mouse.ml" ~doc:"Mouse test" ~requires:[tsdl; affect] ~run:false

let happy_eyeballs =
  let doc = "Happy eyeballs" in
  test ~/"test/happy_eyeballs.ml" ~doc ~requires:[unix; affect_unix] ~run:false

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
      ["effects"; "concurrency"; "parallelism"; "fibers"; "org:erratique"; ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "5.3.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|}; ]
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"affect" ~meta ~locked:true @@
  B0_unit.list ()
