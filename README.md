# Coop

Coop is a prototype programming language for programming with *runners*, also known as *comodels*.

Coop is part of ongoing research by [Danel Ahman](https://danel.ahman.ee) and [Andrej
Bauer](http://www.andrej.com/). Until a proper publication has appeared, you may wish to
consult Danel's talk [Interacting with external resources using runners (aka
comodels)](https://danel.ahman.ee/talks/chocola19.pdf).

For a general background on algebraic effects and handlers we suggest the
lecture notes [What is algebraic about algebraic effects and
handlers?](https://arxiv.org/abs/1807.05923), especially Section 4 which covers
comodels.


## Prerequisites

To compile Coop you need:

* [OCaml](https://ocaml.org) and [OPAM](https://opam.ocaml.org)

* The OPAM packages `menhir`, `sedlex`, and `dune`:

        opam install menhir
        opam install sedlex
        opam install dune

* It is recommended that you also install the `rlwrap` or `ledit` command line wrapper.


## Compilation

You can type:

* `make` to make the `coop.native` executable.
* `make byte` to make the bytecode `coop.byte` executable.
* `make clean` to clean up.
* `make doc` to generate HTML documentation (see the generated [`coop.docdir/index.html`](coop.docdir/index.html)).


# Language features

`[Note: everything below here is obsolete.]`

### Concrete syntax

The concrete syntax of Coop is described in the upcoming paper with Danel Ahman. Have a
look at [examples][./examples] and [tests][./tests]. Note that:

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
* runner types `{Σ} @ t ⇒ {Σ; Θ}`, where `t` is an value type, explained below.

A computation type has the form

     t ! {Σ;Θ}

where `t` is a value type and `{Σ;Θ}` is an effect signature. An expression has
such a type if it computes values of type `t`, possibly performs operations `Σ`
and signals `Θ`.

### Language-specific constructs

#### `runner`

A **runner** has the form

    runner {
      | op₁ x @ w → c₁
      | op₂ x @ w → c₂
        ⋮
    } @ t

where `t` is the type of the state and the clauses

    | opᵢ x @ w → cᵢ

are the **co-operations**. Each co-operation `opᵢ` takes an argument `x` and the
*current state* `w`. The corresponding computation `cᵢ` computes from `x` and
`w` a pair `(v, w')` where `v` is the **co-operation result** and `w'` is the **new
state**.

We think of a runner as defining a (simulated) external environment in which
a piece of code can run. Therefore we refer to the runner state as the
**runner world**. Runners correspond to comodels.

##### Example: the state runner

Assuming we have declared operations

    operation put : int → unit
    operation get : unit → int

the following state runner can be defined:

    let state =
      runner {
      | get () @ z -> (z, z)
      | put y @ _ -> ((), y)
      } @ int

#### `using ⋯ run ⋯ finally { ⋯ }`

A runner `e` with initial state `w` can be used in a computation `c` with the construct:

    using e @ w in
      c
    finally {
    | val x @ w → c_v
    | s₁ x @ w → c₁
    | s₂ x @ w → c₂
      ⋮
    }

When `c` is evaluated, it may use only the operations that `e` handles, i.e.,
we prevent it from calling any outer co-operations (however, the co-operations
of `e` may refer to the outer co-operations).

The `finally` block has a *value clause* and *signal clauses*. The value clause

    | val x @ w → c_v

is used when `c` successfully computes a value, in which case `c_v` is passed
the computed value as `x` and the current world as `w`. It is intended that
`c_v` perform any finalization necessary (such as closing a file or more
generally releasing a resource), and possibly transform the value computed by
`c`.

A signal clause

    | sᵢ x @ w → cᵢ

intercepts signal `sᵢ` raised by a co-operation of `e` or the computation `c`.
It is passed the data `x` carried by the signal and the current world, again
with the intenion that necessary finalization may be performed. The signal
clause may again raise a signal and perform outer co-operations.

##### Example

The above state runner can be used as follows:

    using state @ 42
    run
      let a = get () in
      put (a + 8) ;
      let b = get () in
      a + b
    finally {
      val x @ _ -> x
    }
