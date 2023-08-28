# Multi Threading in Eio

Fibers are run in an **event loop** (`Domain_manager.run_event_loop`), which handles scheduling and interaction with system interfaces like signals, file system, network, etc.
Each Eio backend provides a custom event loop for that operating system.
But an event loop only runs in one domain.
For multi-threading, a `Domain_manager.t` is provided, which can spawn new domains and run event loops in them.
It's also possible to spawn new domains without event loops, these can only be used for computation and cannot interact with I/O (a.d. by that I think they mean just Eio I/O functions)

The `Eio_main.run` function (the main entry point to Eio) gives access to a `Eio.Stdenv`.
This environment contains among others a `Domain_manager.t`.
Some examples of the behavior of the domain manager are given [here](https://github.com/ocaml-multicore/eio/blob/main/tests/domains.md).

## Thread Safety

The main way that fibers communicate (intra or inter domain) is via promises (`Promise`, `Broadcast` & `Cell`).
Promises are either in a waiting state, or completed with an error or a value.
Promises can be shared between threads so they must be thread-safe.
They are implemented via a customized CQS. CQS is already verified thread-safe in Coq, so we should be able to reuse the common parts and prove a specification for the new `resume_all` function.

There are some informal constraints on functions like "must be able to be called in any context". E.g. in `broadcast.mli`.

```
[fn ()] may be called from the caller's context, or by [resume_all],
so it needs to be able to cope with running in any context where that
can run. For example, [fn] must be safe to call from a signal handler
if [resume_all] can be called from one. [fn] must not raise.
```

In practice, the function saved in a cell by `Broadcast` will always be an `enqueue` function that appends a fiber to the run queue of a scheduler.
I interpret "be called from caller's context, or by resume_all [which might be from different domain]" as "must only have persistent preconditions", which is true for `enqueue`.
To be called from a signal handler, it also must not be blocking but I think that is not a safety concern.

## Examples found in docs

Below are some examples where the comments indicate that thread-safety restrictions exist for some functions.
They could be wortwhile to look at for verification work.

- [`Condition.await_no_mutex`](https://github.com/ocaml-multicore/eio/blob/286a1b743d3a55c5318a1301083e311f5b7d5b91/lib_eio/condition.mli#L55)
  Conditions have some unsafe functions that do not use mutexes.
- [Eio.Pool](https://github.com/ocaml-multicore/eio/blob/286a1b743d3a55c5318a1301083e311f5b7d5b91/lib_eio/pool.mli#L29)
  Resource pools have some unsafe interaction with switches.
- [Sched](https://github.com/ocaml-multicore/eio/blob/286a1b743d3a55c5318a1301083e311f5b7d5b91/lib_eio_linux/sched.ml#L55)
  Other domains can change the run queue of a scheduler.
- [Domain_manager.run](https://github.com/ocaml-multicore/eio/blob/main/lib_eio/eio.mli#L94)
  You can spawn new domains from within one fiber.
- [This issue links some more interesting places for thread-safety](https://github.com/ocaml-multicore/eio/issues/387)

## Signal Handlers

You must not call any blocking or suspending code from a signal handler.

Signal handlers seemed interesting as they are similar to asynchronous effects with only an integer payload.
Theoreticlaly, they act much like a parallel thread since they can interrupt the main thread at any moment and run some code, so similar restrictions to shared values apply.

I think signal handlers are never relevant for safety, but since Eio uses them (a.d. TODO where) we could model them, too.
