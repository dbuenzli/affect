Affect — Concurrency and parallelism for OCaml 5
================================================

Affect is a streamlined and natural [concurrency model] for OCaml 5.
It just provides parallel asynchronous function calls with structured
cooperative concurrency and cancellation.

Affect is distributed under the ISC license. It has no dependencies.

Homepage: <https://erratique.ch/software/affect>  

[concurrency model]: https://erratique.ch/software/affect/doc/Fiber/index.html#concurrency_model

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
* [`happy_eyeballs.ml`](test/happy_eyeballs.ml), an implementation
  of a happy eyeballs 

You can run them with `b0 -- ping` or `b0 -- mouse`.
