# Coop Manual

This short manual explains the syntax and the basic concepts of Coop.


**Table of contents**

* [Overview](#overview)
* [Types](#types)
* [Computational effects](#computational-effects)
* [Values](#values)
* [Kernel and user computations](#kernel-and-user-computations)
* [Top-level directives](#top-level-directives)
* [Examples](#examples)

## Overview

Every expression in Coop is either a [*value*](#values) (pure, inert piece of data), or an
*effectful* computation which runs either in [*user mode*](#user-mode) or [*kernel
mode*](#kernel-mode). The user mode is used for "normal" effectful code that may call
operations and raise exceptions. Kernel mode is used to implement resources. It too can
call [operations](#operations), raise and catch [exceptions](#exceptions), but in addition
has access to [kernel state](#kernel-state) (hidden from the user mode) and the ability to
send [signals](#signals), which are unrecoverable exceptions that kill user code.

A central concept in Coop is a [runner](#runners). It is a collection of *co-operations*
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

## Types

### Value types

#### Common values

#### Functions

#### Runners

### Effect annotations

### Kernel and user types

### Type definitions

## Computational effects

### Operations

### Exceptions and signals

#### Exceptions

#### Signals

## Values

## Kernel and user mode

### Common constructs

### User mode

### Kernel mode

#### Kernel state

### Running (virtual machines)


## Top-level directives


## Examples
