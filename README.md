Affect — Composable concurrency primitives for OCaml
====================================================
%%VERSION%%

Affect provides composable concurrency primitives for OCaml using the
effect handlers available in OCaml 5.0.

Affect should be seen as an experiment at that point.

Affect is distributed under the ISC license. It has no dependencies.

Homepage: <https://erratique.ch/software/affect>  

## Installation

Affect can be installed with `opam`: 

    opam pin add affect https://erratique.ch/repos/affect.git

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online][doc] or via `odig doc affect`.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[doc]: https://erratique.ch/software/affect/doc/
[ocaml-forum]: https://discuss.ocaml.org/

## Sample code

A few basic sample programs can be found in the [test](test/)
directory.

* [`ping.ml`](test/ping.ml), client and server using `Funix` to 
  do useless networking on your machine.
* [`mouse.ml`](test/mouse.ml), proof of concept interfacing 
  with the SDL event loop.

You can run them with `b0 -- ping` or `b0 -- mouse`.
