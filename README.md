# Terminus

A toy programming language for programming with comodels.

## Prerequisites

* [OCaml](https://ocaml.org) and [OPAM](https://opam.ocaml.org)

* The OPAM packages `menhir` and `sedlex`:

        opam install menhir
        opam install sedlex

* It is recommended that you also install the `rlwrap` or `ledit` command line wrapper.

## Compilation

You can type:

* `make` to make the `terminus.native` executable.
* `make byte` to make the bytecode `terminus.byte` executable.
* `make clean` to clean up.
* `make doc` to generate HTML documentation (see the generated [`terminus.docdir/index.html`](terminus.docdir/index.html)).

