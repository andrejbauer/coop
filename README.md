# Coop

Coop is a prototype programming language for programming with comodels.

## Prerequisites

* [OCaml](https://ocaml.org) and [OPAM](https://opam.ocaml.org)

* The OPAM packages `menhir` and `sedlex`:

        opam install menhir
        opam install sedlex

* It is recommended that you also install the `rlwrap` or `ledit` command line wrapper.

## Compilation

You can type:

* `make` to make the `coop.native` executable.
* `make byte` to make the bytecode `coop.byte` executable.
* `make clean` to clean up.
* `make doc` to generate HTML documentation (see the generated [`coop.docdir/index.html`](coop.docdir/index.html)).

## Language features

### Concrete syntax

The concrete syntax of Coop mostly follows OCaml syntax, with the following exceptions:

* `match` statements must be terminated with `end`
* function arguments must be explicitly annotated with types
* recursive function definitions must explicitly annotate the return type of the function

There are also new constructs, see below.


### Operations, signals and signatures

There are user-definable operations and signals. An operation is declared as

    operation op : t → u

where `op` is the operation name, `t` is its argument type, and `u` is its
return type. Both `t` and `u` are expression types (see below).

A signal is declared as

    signal sgl of t

where `sgl` is the signal name and `t` is the type of its argument. Signals are
akin to exceptions, except that they follow a different control mechanism, as
described below in the `using` section.

An *operation signature* `Σ` is a set of operation names.

A *(effect) signature* is a pair `Σ, Θ` of an operation signature `Σ` together
with a set `Θ` of signals names.

Coop prints an operation signature as `{Σ}` and a signature as `{Σ;Θ}`. We use
the same notation below.

### Types

Coop has has a static type system with effct types and subtyping, but no
polymorphism. There are *value types*, which are the types of pure
expressions, and *computation types*, which are the types of effectful
expressions.

The value types are:

* primitive types `empty`, `unit`, `bool`, `int`, `string`
* user-definable datatypes
* user-definable abstract types
* function types `t → u` where `t` is an value type and `u` is a computation type
* product types `t₁ * ⋯ * t₂`
* comodel types `{Σ} @ t ⇒ {Σ; Θ}`, where `t` is an value type, explained below.

A computation type has the form

     t ! {Σ;Θ}

where `t` is a value type and `{Σ;Θ}` is an effect signature. An expression has
such a type if it computes values of type `t`, possibly performs operations `Σ`
and signals `Θ`.

### Language-specific constructs

#### Comodels

To be described.

### The `using` construct

To be described.
