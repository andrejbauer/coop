# Coop Manual

This short manual explains the syntax and the basic concepts of Coop.

**Table of contents**

* [Overview](#overview)
* [Computational effects](#computational-effects)
* [Types](#types)
* [Values](#values)
* [Kernel and user computations](#kernel-and-user-computations)
* [Top-level directives](#top-level-directives)
* [Syntax](#syntax)
* [Examples](#examples)

**Note:** Coop recognizes UTF8 characters such as `→`. Every such symbol also has
an ASCII equivalent, e.g., `->`. We use here the UTF8 characters. Please refer
to the [syntax](#syntax) section for the mapping from UTF8 to ASCII equivalents.

## Overview

Every expression in Coop is either a [value](#values) (pure, inert piece of data), or an
*effectful computation* which runs either in [user mode](#user-mode) or [kernel
mode](#kernel-mode). The user mode is used for "normal" effectful code that may call
operations and raise exceptions. Kernel mode is used to implement resources. It too can
call [operations](#operations), raise and catch [exceptions](#exceptions), but in addition
has access to [kernel state](#kernel-state) (hidden from the user mode) and the ability to
send [signals](#signals), which are unrecoverable exceptions that kill user code.

A central concept in Coop is a [runner](#runner). It is a collection of *co-operations*
which implement a resource, such as state or I/O. The co-operations run in kernel mode,
have access to state (local to the runner), may raise exceptions and send signals.
A runner `R` is used to "virtualize" user code `M`, as follows:

    using R @ I
    run
      M
    finally {
      return x @ c → N,
      …
      !e x @ c → N_e,
      …
      ‼s x → N_s,
      …
    }

The code `I` initalizes the runner state, after which `M` is executed. When `M` calls an
operation `op`, the corresponding co-operations `op` in `R` is executed in kernel mode
with access to the state (via the operations `getenv` and `setenv`). The co-operation may:

1. return a value `v` to `M`, which proceeds with execution,
2. raise an exception in `M`, at the call site of the operation, which may or may not be caught
3. send a signal `s`, in which case `M` is dropped and execution proceeds to finalization code `N_s`.

Apart from intercepting signals, the finalization code also intercepts a result returned
by `M` and any exception that percolates out of `M`. The return- and exception-handling
code `N` and `N_e` have acces to the final runner state `c`, with the intent of cleaning
up reasorces, e.g., closing files and releasing memory.

Thus, exceptions should be used for "checked" or "recoverable" exceptions that the user
code can shoudl react to, while signals are sent when normal operation of the runner is
not possible anymore (which is why signal handlers do not receive kernel state).


## Computational effects

Coop has user-definable computational effects, of which there are three kinds:
operations, exceptions, and signals.

### Operations

All computational effects (apart from exceptions and signals) are represented by
operations. You can declare new operation `op` at the top level with the directive

    operation op : τ → ρ {!exc₁, !exc₂, …}

This says that `op` is an operation which takes an argument of type `τ` and
returns a result of type `ρ`, or raises one of the exceptions `exc₁, exc₂, …`. The
shorthand `operation op : τ → ρ` is equivalent to `operation op : τ → ρ {}`.

An operation `op` with argument `v` is called with `op v`. Such calls are
handled by [runners](#runner), or by [containers](#container) at the toplevel.

### Exceptions

Exceptions are used to indicate a *recoverable* error that user code should
react to. A new exception `exc` carrying data of type `τ` is declared at the top
level with the directive

    exception exc of τ

An exception `exc` with argument `v` is raised with `!exc v`. Exceptions may be
caught with [exception handlers](#exception-handling).

### Signals

Signals are used to indicate an *unrecoverable* error that prevents the user
code from continuing. A new signal `sig` carrying data of type `τ` is declared
at the top level with the directive

    signal sig of τ

In kernel mode only, a signal `sig` with argument `v` is sent with `‼sig v`.
Signals cannot be caught, but they may be [finalized](#finalization).

### Effect singatures

An **effect signature** is a set of operations, exceptions and signals:

    { op₁, op₂, …, !exc₁, !exc₂, …, ‼sig₁, ‼sig₂, … }

The operations, exceptions and signals may be listed in any order.

Effect signatures are used to annotate computations with effects that they *may*
perform. The empty effect signature `{}` expresses the fact that a computation
is *pure*, i.e., it performs no effects (but it may run forever).

An **operation signature** `{op₁, op₂, …}` is an effect signature which lists only operations.
Similarly an **exception signature** `{!exc₁, !exc₂, …}` lists only exceptions.

## Types

Coop has **value**, **user** and **kernel types**.

### Value types

Value types classify pure data, i.e., innert expressions that are already
"computed" to their final form, such as `42`, `("foo", false)`, and `fun (x :
int) -> x + 3`.

The value types are:

* the ground types `empty`, `unit`, `bool`, `int`, `string`,
* products `τ₁ × ⋯ × τᵢ`,
* user function types `τ → υ`, where `τ` is a value type and `υ` a [user
  type](#user-and-kernel-types),
* kernel function types `τ → κ` where `τ` is a value type and `κ` is a [kernel
  type](#user-and-kernel-types),
* [runner](#runners) types `Σ ⇒ Σ' @ τ` where `Σ`, `Σ'` are [operation
  signatures](#effect-signatures) and `τ` is a value type,
* [container](#containers) types, where `Σ` is an operation signature.

### User and kernel types

User and kernel types classify possibly effectful computations, i.e.,
expressiosn that need to be computed to give a result, and that may perform
computational effects, such as `3 + 2` and `print_string "Hello, world!"`.

A **user type** has the form

    τ {op₁, op₂, …, !exc₁, !exc₂, …}

where `τ` is a value type. Note that no signals are allowed in the effect
signature of a user type. An expression of this type computes a value of type
`τ`. It may call the listed operations and raised the listed exceptions (and no
others).

A **kernel type** has the form

    τ {op₁, op₂, …, !exc₁, !exc₂, …, ‼sig₁, ‼sig₂, …} @ ρ

where `τ` and `ρ` are value types. An expression of this type computes a value
of type `τ` in kernel mode, where the kernel state has type `ρ`. It may call the
listed operations, raise the listed exceptions, and send the listed signals (and
no others).

### Type definitions

At the top level a **type alias** `t` may be introduced with

    type t = ⋯

Type aliases are transparent, i.e., they are unfolded automatically.

An **algebraic datatype** `t` may be defined with

    type t = C₁ of τ₁ | C₂ of τ₂ | ⋯

The *constructors* `C₁`, `C₂`, ... must be capitalized words. Mutually recursive
algebraic datatype definitions are supported as

    type t₁ = ...
    and  t₂ = ...
    ...

An **abstract type** `t` is declared as

    type t

Such a type has no values. (Exercise: what's it good for?)

## Values

Values are pure data, i.e., they perform no effects and need not be evaluated
any furhter. They are:

* variables
* boolean values `false` and `true`
* algebraic type constructor `C ...`
* numerals `…, -2, -1, 0, 1, 2, …`
* string literal `"..."` 
* tuple `(⟨value₁⟩, …, ⟨valueᵤ⟩)`
* user function abstraction `fun (p : τ) → ⟨user-comp⟩`
* kernel function abstraction `fun (p : τ) @ ρ → ⟨kernel-comp⟩`, where `ρ` is the type of the kernel state
* runner `{ op₁ p₁ → ⟨kernel-comp₁⟩| op₂ p₂ → ⟨kernel-comp₂⟩ | ⋯ } @ ρ`
* runner renaming `⟨value⟩ as {op₁=op₁', op₂=op₂', …}`
* runner pairing `⟨value₁⟩ ⋈ ⟨value₂⟩`

A [**runner**](#runner)

    { op₁ p₁ → ⟨kernel-comp₁⟩ | op₂ p₂ → ⟨kernel-comp₂⟩ | ⋯ } @ ρ

implements operations `op₁, op₂, …` as the given kernel computations with kernel
state of type `ρ`. To distinguish operations from their implementations, we call
the latter **co-operations**. (The name comes from the duality between algebraic
operations and runner co-operations.)

Sometimes we want to create another copy of a runner, with different operation
names, for instance, when we want to pair two copies of the same runner. This is
accomplished with a **runner renaming** `⟨runner⟩ as {op₁=op₁', op₂=op₂', …}`
which takes a runner `⟨runner⟩` and renames some or all of its operations `op₁', op₂', …`
to `op₁, op₂, …`.

Given runners

* `⟨runner₁⟩` with co-operations `op₁, op₂, …` and state `ρ₁`, and
* `⟨runner2⟩` with co-operations `op₁', op₂', …` and state `ρ₂`

the **runer pairing** `⟨runner₁⟩ ⋈ ⟨runner₂⟩` combines them to give a runner
with co-operations `op₁, op₂, …, op₁', op₂', …` and state `ρ₁ × ρ₂`.

## Kernel and user computations

### Common constructs

#### `let`-binding

#### Recursive definitions

#### Exception handling


### User mode

#### Finalization

### Kernel mode

The following computations are specific to kernel mode:

* `kill s` sends a signal
* `getenv` reads the current kernel state
* `setenv ⟨expr⟩` sets the state to `⟨expr⟩`
* `user` context switch




### Running (virtual machines)


## Top-level directives

### Containers

Need a [container](#container).

## Syntax

## Examples
