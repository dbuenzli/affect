#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let cmdliner = Conf.with_pkg "cmdliner"
let () =
  Pkg.describe "affect" @@ fun c ->
  let cmdliner = Conf.value c cmdliner in
  Ok [ Pkg.mllib "src/affect.mllib";
       Pkg.lib "src/affect_top_init.ml";

       Pkg.mllib "src/unix/affect_unix.mllib" ~dst_dir:"unix";
       Pkg.clib "src/unix/libaffect_unix_stubs.clib";
       Pkg.lib "src/unix/affect_unix_top_init.ml"
         ~dst:"unix/affect_unix_top_init.ml";

       Pkg.mllib "src/tmp/affect_tmp.mllib" ~dst_dir:"tmp";
       Pkg.lib "src/tmp/affect_tmp_top_init.ml"
         ~dst:"tmp/affect_tmp_top_init.ml";

       Pkg.mllib ~cond:cmdliner ~dst_dir:"cli" "src/cli/affect_cli.mllib";

       Pkg.doc "doc/index.mld" ~dst:"odoc-pages/index.mld";
       Pkg.doc "doc/design.mld" ~dst:"odoc-pages/design.mld";
       Pkg.doc "doc/cookbook.mld" ~dst:"odoc-pages/cookbook.mld";
       Pkg.doc "doc/concurrency_model.mld"
         ~dst:"odoc-pages/concurrency_model.mld";
     ]
