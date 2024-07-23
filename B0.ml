open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

let b0_std = B0_ocaml.libname "b0.std"
let unix = B0_ocaml.libname "unix"

let affect = B0_ocaml.libname "affect"
let affect_unix = B0_ocaml.libname "affect.unix"

(* Libraries *)

let src f = `File Fpath.(v "src" / f)

let affect_lib =
  let srcs = [src "fiber.mli"; src "fiber.ml"] in
  let requires = [] in
  B0_ocaml.lib affect ~doc:"The affect library" ~srcs ~requires

let affect_unix_lib =
  let srcs = [src "funix.mli"; src "funix.ml";
              src "netmsg.mli"; src "netmsg.ml"]
  in
  let requires = [unix; affect] in
  B0_ocaml.lib affect_unix ~doc:"The affect.unix library" ~srcs ~requires

(* Tests *)

let test_src f = `File Fpath.(v "test" // f)
let test ?run:(r = false) ?(requires = []) ?(more_srcs = []) file ~doc =
  let file = Fpath.v file in
  let srcs = test_src file :: more_srcs in
  let meta = B0_meta.(empty |> tag test |> ~~ run r) in
  let requires = affect :: requires in
  B0_ocaml.exe (Fpath.basename ~strip_ext:true file) ~doc ~srcs ~requires ~meta

let test_affect =
  test "test.ml" ~doc:"affect tests" ~run:true ~requires:[b0_std]

let test_unix =
  let requires = [b0_std; unix; affect_unix] in
  test "test_funix.ml" ~doc:"affect.unix tests" ~requires ~run:true

let test_busy =
  let requires = [unix; affect_unix] in
  test "test_busy.ml" ~doc:"Too much CPU used!" ~requires

let ping = test "ping.ml" ~doc:"Ping-pong test" ~requires:[unix; affect_unix]
let mouse =
  let tsdl = B0_ocaml.libname "tsdl" in
  test "mouse.ml" ~doc:"Mouse test" ~requires:[tsdl; affect]

let happy_eyeballs =
  let doc = "Happy eyeballs" in
  test "happy_eyeballs.ml" ~doc ~requires:[unix; affect_unix]

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
      ["effects"; "concurrency"; "fibers"; "org:erratique"; ]
    |> ~~ B0_opam.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> ~~ B0_opam.depends
      [ "ocaml", {|>= "5.2.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|}; ]
    |> B0_meta.tag B0_opam.tag
  in
  B0_pack.make "default" ~doc:"affect" ~meta ~locked:true @@
  B0_unit.list ()
