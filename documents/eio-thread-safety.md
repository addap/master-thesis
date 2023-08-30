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

There are two datastructures in Eio that must be thread safe: the run queue of a scheduler and a customized CQS (cancellable concurrent queue).
There are also other datastructures built on top of CQS like promises, [semaphores](https://github.com/ocaml-multicore/eio/blob/387fb6d2b9cb6bf03775c432b880f6ab6604e8ce/lib_eio/sem_state.ml), [synchronous channels](https://github.com/ocaml-multicore/eio/blob/387fb6d2b9cb6bf03775c432b880f6ab6604e8ce/lib_eio/sync.mli) & [resource pools](https://github.com/ocaml-multicore/eio/blob/387fb6d2b9cb6bf03775c432b880f6ab6604e8ce/lib_eio/pool.ml).

The run queue of a scheduler uses an `Lwt` thread-safe queue.
I did not see a formal verification of it and in the proofs so far it is axiomatized.
Actually, it could be replaced by a CQS queue if we are fine with a different scheduling behavior (with the Lwt queue it sometimes enqueues at the front or at the back, but this is not possible with CQS. This change would not affect safety.)

Instead, we should focus on the customized CQS because it is used to implement promises.
Promises are the main way that fibers communicate intra & inter domain, so they are inherent to modeling fibers.

My favored approach would then be to verify the customized parts of CQS (in module `Cell`) and take the unchanged parts as axioms (because the proofs are huge).
Then we can verify promises (in modules `Promise` & `Broadcast`) built on top of CQS.
Verifying the other abstractions built on top of CQS would be a bonus but it's not part of the core fiber abstraction, so low priority.
If we want, we can build a simple standard queue on top of CQS and use that instead of the axiomatized Lwt queue.

### Customized CQS

We discuss the structure of CQS and how Eio customized it [here](./eio-cqs.md).

## Thread safety hinted at in docs

There are some informal constraints on functions like "must be able to be called in any context". E.g. in `broadcast.mli`.

```
[fn ()] may be called from the caller's context, or by [resume_all],
so it needs to be able to cope with running in any context where that
can run. For example, [fn] must be safe to call from a signal handler
if [resume_all] can be called from one. [fn] must not raise.
```

In practice, the function saved in a cell by `Broadcast` will always be an `enqueue` function that appends a fiber to the run queue of a scheduler.
I interpret "be called from caller's context, or by resume_all [i.e. from different domain]" as "must only have persistent preconditions", which is true for `enqueue`.
To be called from a signal handler, it also must not be blocking but I think that is not a safety concern.

Below are some examples where the comments indicate that thread-safety restrictions exist for some functions.
They could be wortwhile to look at for verification work.

- [`Condition.await_no_mutex`](https://github.com/ocaml-multicore/eio/blob/286a1b743d3a55c5318a1301083e311f5b7d5b91/lib_eio/condition.mli#L55)
  Conditions have some unsafe functions that do not use mutexes.
- [Eio.Pool](https://github.com/ocaml-multicore/eio/blob/286a1b743d3a55c5318a1301083e311f5b7d5b91/lib_eio/pool.mli#L29)
  Resource pools have some unsafe interaction with switches.
- [Sched](https://github.com/ocaml-multicore/eio/blob/286a1b743d3a55c5318a1301083e311f5b7d5b91/lib_eio_linux/sched.ml#L55)
  Other domains can change the run queue of a scheduler (but we assume the Lwt queue is thread-safe)
- [Domain_manager.run](https://github.com/ocaml-multicore/eio/blob/286a1b743d3a55c5318a1301083e311f5b7d5b91/lib_eio/eio.mli#L89)
  You can spawn new domains from within one fiber, but these must only access thread-safe values from the spawning domain.
- [This issue links some more interesting places for thread-safety](https://github.com/ocaml-multicore/eio/issues/387)

## Signal Handlers

You must not call any blocking or suspending code from a signal handler.

Signal handlers seemed interesting as they are similar to asynchronous effects with only an integer payload.
Theoreticlaly, they act much like a parallel thread since they can interrupt the main thread at any moment and run some code, so similar restrictions to shared values apply.

I think signal handlers are never relevant for safety, but since Eio uses them (a.d. TODO where) we could model them, too.
