open Ocamlbuild_plugin
open Command

let os =
  String.trim @@
  try Sys.getenv "HOST_OS"
  with Not_found -> if Sys.win32 then "Win32" else run_and_read "uname -s"

let lib s = match !Ocamlbuild_plugin.Options.ext_lib with
 | "" -> s ^ ".a"
 | x -> s ^ "." ^ x

let system_support_lib = match os with
| "Linux" -> [A "-cclib"; A "-lrt"]
| _ -> []

let () =
  dispatch begin function
  | After_rules ->
      dep ["compile";"c"]
          ["src/unix/affect_unix_stubs.h"];

      dep ["record_affect_unix_stubs"] [lib "src/unix/libaffect_unix_stubs"];

      flag_and_dep
        ["link"; "ocaml"; "link_affect_unix_stubs"]
        (P (lib "src/unix/libaffect_unix_stubs"));

      flag ["library"; "ocaml"; "byte"; "record_affect_unix_stubs"]
        (S ([A "-dllib"; A "-laffect_unix_stubs"] @ system_support_lib));

      flag ["library"; "ocaml"; (* byte and native *)
            "record_affect_unix_stubs"]
        (S ([A "-cclib"; A "-laffect_unix_stubs"] @ system_support_lib));

      ocaml_lib ~tag_name:"use_affect_unix_stubs"
        ~dir:"src" "src/unix/affect_unix";

      flag ["link"; "ocaml"; "use_affect_unix_stubs"]
        (S [A "-ccopt"; A "-Lsrc"]);
  | _ -> ()
  end
