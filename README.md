# Coop

Coop is a prototype programming language for programming with *runners*, also known as *comodels*.

Coop is part of ongoing research by [Danel Ahman](https://danel.ahman.ee) and
[Andrej Bauer](http://www.andrej.com/). The theoretical aspects of our work are
described in the paper [Runners in action](https://arxiv.org/abs/1910.11629).
You may also be interested in Danel's talk [Interacting with external resources
using runners (aka comodels)](https://danel.ahman.ee/talks/chocola19.pdf) and
his [Haskell-Coop](https://github.com/danelahman/haskell-coop) library.


## Installation

### Prerequisites

To compile Coop you need:

* [OCaml](https://ocaml.org) and [OPAM](https://opam.ocaml.org)

* The OPAM packages `menhir`, `sedlex`, and `dune`:

        opam install menhir
        opam install sedlex
        opam install dune

* It is recommended that you also install the `rlwrap` or `ledit` command line wrapper.

### Obtaining Coop

If you're reading this file then you have probably found the [Coop GitHub
repository](https://github.com/andrejbauer/coop), where Coop is available.

### Compilation

You can type:

* `make` to make the `coop.native` executable.
* `make byte` to make the bytecode `coop.byte` executable.
* `make clean` to clean up.
* `make doc` to generate HTML documentation (see the generated [`coop.docdir/index.html`](coop.docdir/index.html)).

## Language features

To find out more about Coop, please consult the [Coop manual](./Manual.md).

### Theoretical background

Coop is an extension of `λ-coop`, a calculus devised by Danel Ahman and Andrej
Bauer, cf. our paper [Runners in action](http://arxiv.org/abs/1910.11629), to
study how *runners* (also known as *comodels*) can be used to program with
external resources. These are similar to handlers for algebraic effects, except
that they carry state and use the continuation at most one in a tail-call
position. There are two kinds of computations in `λ-coop`: the *kernel*
computations implement access to resources as *co-operations*, while the *user*
computations use resources by calling algebraic operations. The kernel mode has
the ability to report errors to user mode by raising *exceptions* (recoverable
errors), or aborting user code by sending it *signals* (unrecoverable errors).

The central construct of `λ-coop` is

    using R @ I run
      M
    finally F

which runs user code `M` and handles its operations using the (kernel mode) co-operations
of runner `R`. It is a kind of "virtual machine" which runs user code inside a kernel
represented by `R`. The runner has access to state (hidden from the user code) which is
initialized by `I`. The finalisation code `F` intercepts the return value, exceptions and
signals, with the purpose of properly disposing of any resources used by the runner `R`.
The calculus guarantee that `F` will be reached (unless an outer runner kills the present
one).

The type system of `λ-cup` keeps track of effects and supports subtyping.

## Acknowledgements

<table>
      <tr><td>This project has received funding from the European Union’s Horizon 2020 research and innovation programme under the Marie Skłodowska-Curie grant agreement No 834146.</td><td><img src="https://danel.ahman.ee/images/eu_flag.jpg"></td></tr>
      <tr><td>This material is based upon work supported by the Air Force Office of Scientific Research under award number FA9550-17-1-0326.</td><td></td></tr>
</table>
