#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "affect" @@ fun c ->
  Ok [ Pkg.mllib "src/affect.mllib";
       Pkg.mllib "src/unix/affect_unix.mllib" ~dst_dir:"unix";
       Pkg.lib "src/affect_top_init.ml";
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld"
       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/design.mld"]
