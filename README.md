Affect — Streamlined and natural concurrency model for OCaml
==============================================================

Affect is a streamlined and natural concurrency model for OCaml.

It provides parallel asynchronous functions and first-class
synchronous actions to orchestrate them. The resulting [concurency
model] has structured cooperative concurrency, structured cancellation
and structured effect handling.

Affect is distributed under the ISC license. The base library has no
dependencies. It optionally depends on the [`cmdliner`] library and
the OCaml `unix` library.

Homepage: <https://erratique.ch/software/affect/>

[`cmdliner`]: <http://erratique.ch/software/cmdliner>
[concurrency model]: https://erratique.ch/software/affect/doc/concurrency_model.html

## Installation

Affect can be installed with `opam`: 

    opam install affect

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.

## Documentation

The documentation can be consulted [online][doc] or via `odig doc affect`.

Questions are welcome but better asked on the [OCaml forum][ocaml-forum] 
than on the issue tracker.

[doc]: https://erratique.ch/software/affect/doc/
[ocaml-forum]: https://discuss.ocaml.org/

## Examples

A few examples can be found in the [test](test/) directory. 

## Acknowledgements 

A grant from the [OCaml Software Foundation] helped to bring the first
public release of `affect`.

[OCaml Software Foundation]: http://ocaml-sf.org/
