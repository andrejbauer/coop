# Coop Manual

This short manual explains the syntax and the basic concepts of Coop.

**Table of contents**

* [Overview](#overview)
* [Running coop programs](#running-coop-programs)
* [Computational effects](#computational-effects)
  * [Operations](#operations)
  * [Exceptions](#exceptions)
  * [Signals](#signals)
  * [Effect signatures](#effect-signatures)
* [Types](#types)
  * [Value types](#value-types)
  * [User and kernel types](#user-and-kernel-types)
  * [Type definitions](#type-definitions)
* [Values](#values)
  * [Functions](#functions)
  * [Runners](#runners)
  * [Patterns](#patterns)
* [User and kernel computations](#user-and-kernel-computations)
  * [Common computations](#common-computations)
  * [User mode](#user-mode)
  * [Kernel mode](#kernel-mode)
* [Top-level directives](#top-level-directives)
  * [External values](#external-values)
  * [Containers](#containers)
* [Syntax](#syntax)
  * [UTF and ASCII](#utf-and-ascii)

**Note:** Coop recognizes UTF8 characters such as `→`, `⇒` and `⋈`. Every such symbol also
has an ASCII equivalent, e.g., `->`, `=>`, `><`. We use here the UTF8 characters. Please
refer to the [UTF & ASCII](#utf-and-ascii) section for the mapping from UTF8 to ASCII equivalents.

## Overview

Every expression in Coop is either a [value](#values) (pure, inert piece of data), or an
*effectful computation* which runs either in [user mode](#user-mode) or [kernel
mode](#kernel-mode). The user mode is used for "normal" effectful code that may call
operations and raise exceptions. Kernel mode is used to implement resources. It too can
call [operations](#operations), raise and catch [exceptions](#exceptions), but in addition
has access to [kernel state](#kernel-state) (hidden from the user mode) and the ability to
send [signals](#signals), which are unrecoverable exceptions that kill user code.

A central concept in Coop is a [runner](#runners). It is a collection of *co-operations*
which implement a resource, such as state or I/O. The co-operations run in kernel mode,
have access to state (local to the runner), may raise exceptions and send signals. A
runner `R` is used to "virtualize" user code `M` with a [`run` statement](#run-statement)

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

## Running coop programs

Coop programs are saved in files with extension `.coop`. The command

    coop ⟨file.coop⟩

runs the program in the given file. The file may load other files with the [`load`
directive](#top-level-directives).

To get the Coop REPL, run

    coop

If you install the `rlwrap` or `ledit` command, `coop` will warp itself in it to give the
REPL line-editing capabilities. You may also run the Coop REPL with a preloaded file

    coop -l ⟨file.coop⟩

When you run `coop` it will look for the file [`pervasives.coop`](./pervasives.coop) and
load it if found. This behavior can be controled with the `--no-pervasives` and
`--pervasives ⟨file⟩` command-line options. For other command-line options, run `coop --help`.


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
handled by [runners](#runners), or by [containers](#containers) at the toplevel.

#### Example

The file `pervasives.coop` delcares the I/O operations

    operation print_int : int → unit
    operation print_string : string → unit
    operation read_int : unit → int {!malformed_integer}
    operation read_string : unit → string
    operation flush : unit → unit

Notice that `read_int` may throw a `malformed_integer` exception. Coop keeps track of
exceptions and issues a warning if the `malformed_integer` exception is not handled.


### Exceptions

Exceptions are used to indicate a *recoverable* error that user code should
react to. A new exception `exc` carrying data of type `τ` is declared at the top
level with the directive

    exception exc of τ

An exception `exc` with argument `v` is raised with `!exc v`. Exceptions may be
caught with [exception handlers](#exception-handling).

#### Example

The file `pervasives.coop` declares the exceptions

    exception division_by_zero of unit
    exception malformed_integer of unit

### Signals

Signals are used to indicate an *unrecoverable* error that prevents the user
code from continuing. A new signal `sig` carrying data of type `τ` is declared
at the top level with the directive

    signal sig of τ

In kernel mode only, a signal `sig` with argument `v` is sent with `‼sig v`.
Signals cannot be caught, but they may be [finalized](#run-statement).


### Effect signatures

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

#### Runner types

A **runner type** has the form

    {op₁, op₂, …} ⇒ {op₁', op₂', …} @ ρ

It classifies [runners](#runners) which implement co-operations `op₁, op₂, …`, use state
of type `ρ` and call operations `op₁', op₂', …`

#### Container types

A container type has the form

    {op₁, op₂, …}

It classifies [containers](#containers) which provide the given operations.


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

#### Example

Executing a kernel comptuations of type

    int ! {print, fopen, !permission_denied, ‼device_error}

results in one of the following:

* an integer return value
* an operation call `print` or `fopen`
* the exception `permision_denied`
* the signal `device_error`



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

#### Example

The type of integer lists may be defined as

    type int_list = Nil | Cons of int * int_list


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

Values can be deconstructed with [patterns](#patterns) in [`let`-binding](#let-binding)
and [`match`-statements](#match-statement).


### Functions

Because Coop does not have polymorphic types, all function arguments must be explicitly
typed. That is, `fun x → x` is not a valid expression, you have to write
`fun (x : τ) → x` for some value type `τ`.

Iterated user functions `fun (x₁ : τ₁) → fun (y : σ) → ⟨user-comp⟩` can be abbreviated to
`fun (x : τ) (y : σ) -> ⟨user-comp⟩`. However, there is no such abbreviation for kernel
functions so you have to write them as `fun (x₁ : τ₁) @ ρ₁ → fun (y : σ) @ ρ₂ → ⟨kernel-comp⟩`.

**Caveat:** you cannot apply a user function in kernel mode. For instance, writing `3 + 4`
in the definition of a [runner co-operation](#runners) is a type error because `+` is a
user function. You have to wrap the user computation in a [`user` context
switch](#user-context-switch) `user 3 + 4 with {}`. (Yes, there could be syntactic sugar
for this sort of thing, and there could be promotion of pure computations to either mode.
It's a *prototype* language!)


### Runners

A runner

    { op₁ p₁ → ⟨kernel-comp₁⟩ | op₂ p₂ → ⟨kernel-comp₂⟩ | ⋯ } @ ρ

implements operations `op₁, op₂, …` as the given kernel computations with kernel
state of type `ρ`. To distinguish operations from their implementations, we call
the latter **co-operations**. (The name comes from the duality between algebraic
operations and runner co-operations.)

#### Runner renaming

Sometimes we want to create another copy of a runner, with different operation names, for
instance, when we want to pair two copies of the same runner. This is accomplished with a
**runner renaming** `⟨runner⟩ as {op₁=op₁', op₂=op₂', …}`
which takes a runner `⟨runner⟩` and renames some or all of its operations `op₁', op₂', …`
to `op₁, op₂, …`.

#### Runner pairing

Given runners

* `⟨runner₁⟩` with co-operations `op₁, op₂, …` and state `ρ₁`, and
* `⟨runner2⟩` with co-operations `op₁', op₂', …` and state `ρ₂`

the **runer pairing** `⟨runner₁⟩ ⋈ ⟨runner₂⟩` combines them to give a runner with
co-operations `op₁, op₂, …, op₁', op₂', …` and state `ρ₁ × ρ₂`. Each component of the
paired runner has access to its own componetn of the state. The opertion names must be
disjoint, which can always be achieved with a [runner renaming](#runner-renaming).

### Patterns

Values can be deconstructed using patterns, as is customary in functional languages. A
pattern `p` may appear anywhere a value is bound:

* argument of a function: `fun (p : τ) → ⋯` and `fun (p : τ) @ ρ → ⋯`
* argumetn of a `return`: `return p → ⋯`
* argument of a co-operation: `op p → ⋯`
* argument of an exception: `!exc p → ⋯` and `!exc p₁ @ p₂ → ⋯`
* argument of a signal: `‼sig p → ⋯`
* in [`let`-binding](#let-binding)
* in [`match`-statements](#match-statement)
* in [recursive functions](#recursive-functions)

Coop does *not* perform pattern exhaustiveness checks. A runtime error occurs if a value
is unsuccessfully matched.

The following patterns are supported:

* anonymous pattern `_` matches everything
* variable bindind `x` matches everything and binds to `x`
* primitive constants: numerals, booleans and string literals
* tuple pattern `(p₁, …, pᵢ)`
* datatype constructor pattern `C p`


## User and kernel computations

Effectful source code running inside a runtime environment is just one example of a more
general phenomenon in which effectful computations are enveloped by a layer that provides
a supervised access to external resources: a user process is controlled by a kernel, a web
page by a browser, an operating system by hardware, or a virtual machine, etc. We adopt
the parlance of software systems, and refer to the two layers generically as the **user**
and **kernel computations**.

### Common computations

Many constructs are common both modes.

#### Pure computations

A value is considered to be a computation and is automatically promoted to one.

In fact, Coop allows the programmer to freely mix values and computations. For example you
can write `(3 + 4, 8)` even though, strictly speaking the subcomputation `3 + 4` should be
hoisted: `let x = 3 + 4 in (x, 8)`. Coop performs such hoisting automatically, as it would
be quite annoying to have to write `let x' = f x in g y` instead of `f x y`.

#### `let` binding

An ML-style `let`-binding has the form

    let ⟨pattern⟩ = ⟨computation₁⟩ in ⟨computation₂⟩

There is also the parallel `let`-binding

    let ⟨pattern₁⟩ = ⋯
    and ⟨pattern₂⟩ = ⋯
    ⋯
    in ⟨computation⟩

At the top level a value can be bound globally with

    let ⟨pattern⟩ = ⟨computation⟩ ;;

and similarly for parallel `let`-binding.


#### `match` statement

An ML-style `match` statement (known as `case` in Haskell) has the form

    match ⟨value⟩ with {
    | ⟨pattern₁⟩ →  ⟨computation₁⟩
    | ⟨pattern₂⟩ →  ⟨computation₂⟩
      ⋯
    }

As an extreme case, `match ⟨value⟩ with { }` eliminates a value of type `empty`.
(Exercise: why is thus useful?)

#### Conditional statement

A conditional statement is written as

    if ⟨boolean-value⟩ then ⟨computation₁⟩ else ⟨computation₂⟩

and it has the usual meaning.

#### Recursive functions

The definition of a recursive function `f` of type `τ → σ` is written as

    let rec f (p : τ) : σ = ⟨computation⟩
    in ⋯

Whether this defines a user or a kernel function depends on `σ` being [a user or a
kernel type](#user-and-kernel-types). Mutual recursive definition are supported:


    let rec f₁ (p₁ : τ₁) : σ₁ = ⟨computation₁⟩
        and f₂ (p₂ : τ₂) : σ₂ = ⟨computation₂⟩
        ...
    in ⋯

At the top level a global recursive function definition can be given, analogously to a
global `let`-binding.

**Caveat:** To define a recursive function of several arguments, say `f : τ₁ → τ₂ → σ` you
have to write

    let f (x₁ : τ₁) : τ₂ → σ = fun (x₂ : τ_2) → ⋯

The following is *not* supported:

    let f (x₁ : τ₁) (x₂ : τ₂) : σ = ⋯

#### Raising an exception

An exception `exc` is raised by

    !exc ⟨value⟩


#### Exception handling

Exception handling takes the form

    try
      ⟨computation⟩
    with {
    | return p → ⋯
    | !exc₁ p₁ → ⋯
    | !exc₂ p₂ → ⋯
    }

The optional `return` clause handles the value returned by the enveloped `⟨computation⟩`.
Any exception that is not listed in the `with` block will pass through it.

### User mode

The following computations are specific to user mode:

* `run` a computation using a runner
* `kernel` context switch

#### `run` statement

The comptuation

    using ⟨runner⟩ @ ⟨value⟩ run
      ⟨user-comp⟩
    finally {
    | return p @ p' → ⟨user-comp⟩
    | !exc₁ p₁ @ p' → ⟨user-comp₁⟩
    | !exc₂ p₂ @ p' → ⟨user-comp₂⟩
      ⋯
    | ‼sig₃ p₃ → ⟨user-comp₃⟩
    | ‼sig₄ p₄ → ⟨user-comp₄⟩
      ⋯ }

runs a user `⟨user-comp⟩` using the co-operations implemented by the `⟨runner⟩` with
initial state `⟨value⟩`. The `finally` clasues handle any exceptions and signals that may
occur during the computation, as well as the returned value. Note that the `return` and
exception clasues also get access to the final state, bound by the pattern `p'`.

**Note:** The `return` clause in `finally` is *mandatory*, and it must finalize *all*
exceptions and signals that may occur.

It is useful to think of the `run` statement as a "virtual machine". The inner computation
`⟨user-comp⟩` can access external resources *only* through the co-operations provided by
the runner.

The finalization code is guaranteed to execute, unless a co-operation in the `⟨runner⟩`
calls an outer operation that is handled by an outer `run` statement which then send a
signal. That is, suppose we have:

    using { op₁ x → ⟨kernel-comp₁⟩ } @ v₁ run
      using { op₂ x → ⟨kernel-comp₂⟩ } @ v₂ run
         ⟨user-comp⟩
      finally {
      | return x₁ @ c₁ → ⋯
      | ‼sig₁ y₁ → ⟨user-comp₁⟩ }
    finally {
      | return x₂ @ c₂ → ⋯
      | ‼sig₁ y₂ → ⟨user-comp₂⟩ }

The inner `finally` will be executed *unless* `⟨kernel-comp₂⟩` calls the operation `op₁`
upon which `⟨kernel-comp₁⟩` sends a signal. In this case the control will be passed
directly to the outer `finally`.

If you think that an inner `finally` should get a chance, then you should not be raising a
signal but rather throwing an exception.

#### `kernel` context switch

It is sometimes necessary to run kernel code inside user mode. The user comptuation

    kernel
      ⟨kernel-comp⟩ @ ⟨value⟩
    finally { ⋯ }

runs kernel code `⟨kernel-comp⟩` at initial state `⟨value⟩`. Any results, exceptions, or
signals are finalized using the `finally` clauses, as in the [`run`
statement](#run-statement). The computation `⟨kernel-comp⟩` may call operations. These
will propagate outwards through the `kernel` switch to the closes enveloping `run`
statement.

### Kernel mode

The following computations are specific to kernel mode:

* `kill s` sends a signal
* `getenv` reads the current kernel state
* `setenv ⟨expr⟩` sets the state to `⟨expr⟩`
* `user` context switch

#### Kernel state

A kernel computation has access to state with operations `getenv` and `setenv ⟨value⟩`,
which read and write the state. In a [runner](#runners) the state is shared among the
co-operations and initialized in the [`run` statement](#run-statement). The kernel state
is also initialized when a kernel computation is executed with thea [`kernel` context
switch](#kernel-context-switch)

The final kernel state is available during finalization in `finally` clasues for `return`
and excepptions (but not signals) of a `run` or `kernel` computation.


#### Sending a signal

In kernel mode a signal `sig` is sent with

    ‼sig ⟨value⟩

Such a signal cannot be caught, but it can be finalized by [`kernel`](#kernel-context-switch) or [`run`](#run-statement).

#### `user` context switch

It is sometimes necessary to run user code inside kernel mode. The kernel computation

    user
      ⟨user-comp⟩
    with { ⋯ }
    | return p → ⋯
    | !exc₁ p₁ → ⋯
    | !exc₂ p₂ → ⋯
    }

The `with` clauses work the same way as in [exception handling](#exception-handling).
In particular, and non-handled exception will pass through it.


## Top-level directives

At the top level the following directives are available:

* `load "⟨file-name⟩"` loads and evaluates the contents of a file
* top-level [`let`-binding](#let-binding)
* top-level [recursive function definition](#recursive-functions)
* [type definitions](#type-definitions)
* declaration of [operations](#operations), [exceptions](#exceptions), and [signals](#signals)
* declaration of an [`external` value](#external-values)
* `container ⟨value⟩` sets the current [container](#containers)
* a user computation `⟨user-comp⟩` may be executed

At the top level, the directives are separated with `;;`. The separator may be omitted when the next directive starts with a keyword that allows the parser to tell that a new directive has started.

### External values

The top level directive

    external x : τ = "⟨name⟩"

binds the variable `x` to an external value `⟨name⟩` of type `τ`. The available external
values are defined in [`external.ml`](src/external.ml).

### Containers

A container is a "top level [runner](#runners)" which gives the computations
access to *actual* computational effects. From the point of view of Coop code,
containers provide co-operations that return values and raise exceptions, but is
unware of any signals that co-operations may send. This is so because an
external (OCaml-level) co-operation runs in the "external OCaml monad" where
such signals live.

Currently Coop provides three containers, defined in OCaml and bound as [external values](#external-values):

* `pure` implements *no* operations
* `stdio` implements basic I/O operations
* `file` implements basic file operations

Please consult [`pervasives.coop`](./pervasives.coop) for details on the `stdio` and `file` containers,
as well as the [`file_operations.coop`](./examples/file_operations.coop) example.

At the top level you can set the current containers with the directive

    container ⟨container₁⟩, …, ⟨containerᵢ⟩

You may use any combination of containers, as long as they have disjoint sets of co-operations.

The initial container is `pure`, which forces Coop programs to be pure. If you
want to use I/O you must first set the `stdio` container.

## Syntax

Coop source code should be saved in files with extension `.coop`.

The following lexical conventions are in place:

* Variable names start with lower-case letters.
* Datatype constructors start with upper-case letters.
* Coop is case-sensitive.
* Code need not be properly indented, which is not to say that it should not be.

### UTF and ASCII

Source code should be UTF8 encoded. If you do not have convenient ways of entering UTF8
(isn't it kind of said that this is a problem in the 3rd millenium?) then you may use the following
ASCII equivalents:

| Operator      | UTF8 | ASCII |
|---------------|------|-------|
| function type | `→`  | `->`  |
| product type  | `×`  | `*`   |
| runner type   | `⇒`  | `=>`  |
| signal        | `‼`  | `!!`  |

