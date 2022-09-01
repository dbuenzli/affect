open B0_kit.V000
open Result.Syntax

(* OCaml library names *)

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
let test_exe ?(requires = []) ?(more_srcs = []) file ~doc =
  let file = Fpath.v file in
  let srcs = test_src file :: more_srcs in
  let meta = B0_meta.(empty |> tag test) in
  let requires = affect :: requires in
  B0_ocaml.exe (Fpath.basename ~no_ext:true file) ~doc ~srcs ~requires ~meta

let test = test_exe "test.ml" ~doc:"affect tests"

let test_unix =
  let requires = [unix; affect_unix] in
  test_exe "test_funix.ml" ~doc:"affect.unix tests" ~requires

let ping =
  test_exe "ping.ml" ~doc:"Ping-pong test" ~requires:[unix; affect_unix]

let mouse =
  let tsdl = B0_ocaml.libname "tsdl" in
  test_exe "mouse.ml" ~doc:"Mouse test" ~requires:[tsdl; affect]

let happy_eyeballs =
  let doc = "Happy eyeballs" in
  test_exe "happy_eyeballs.ml" ~doc ~requires:[unix; affect_unix]

(* Packs *)

let default =
  let meta =
    let open B0_meta in
    empty
    |> add authors ["The affect programmers"]
    |> add maintainers ["Daniel Bünzli <daniel.buenzl i@erratique.ch>"]
    |> add homepage "https://erratique.ch/software/affect"
    |> add online_doc "https://erratique.ch/software/affect/doc"
    |> add licenses ["ISC"]
    |> add repo "git+https://erratique.ch/repos/affect.git"
    |> add issues "https://github.com/dbuenzli/affect/issues"
    |> add description_tags ["effects"; "concurrency"; "fibers";
                             "org:erratique"; ]
    |> add B0_opam.Meta.build
      {|[["ocaml" "pkg/pkg.ml" "build" "--dev-pkg" "%{dev}%"]]|}
    |> tag B0_opam.tag
    |> add B0_opam.Meta.depends
      [ "ocaml", {|>= "5.0.0"|};
        "ocamlfind", {|build|};
        "ocamlbuild", {|build|};
        "topkg", {|build & >= "1.0.3"|}; ]
  in
  B0_pack.v "default" ~doc:"affect" ~meta ~locked:true @@
  B0_unit.list ()
