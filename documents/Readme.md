# Verifying (a simplified version of) the Eio library

The topic of ehis thesis is to verify a simplified version of the Eio library for OCaml 5.
The recently released OCaml 5 includes several new features like multi-threading support and an effects sytem.  
[Eio](https://github.com/ocaml-multicore/eio/) is a concurrency library for OCaml 5 that uses the effect system to model fibers (individual threads of execution).
Eio aims to be the standard concurrency library for OCaml 5.
Compared to previous concurrency libraries like Lwt and Async (which have a monadic interface), concurrency based on effects is easier to use because it is more composable, i.e. you can compose concurrent code with any non-concurrent code.  
[Hazel](https://gitlab.inria.fr/cambium/hazel) is a formalization of an effect system in Iris.
It has been used in multiple case studies, among them a cooperative scheduler implementing threads of execution with effects.
We want to extend this case study to a more real-world scope and verify a scheduler based on the Eio library.

## Definitions

Some explanations of terms we use.

- Execution context: General term for something that executes code with some environment.
- Concurrency: having multiple execution contexts run concurrently.  
  Examples are async/await, "green" or "light-weight" threads, coroutines, etc.
  Concurrent execution contexts do not necessarily run at the same time.
  Instead, concurrency is a way to structure a program which executes multiple tasks to switch to a different task if one gets blocked.
  Concurrency is often used for I/O-bound workloads to execute a different task while another one is blocked waiting for the completion of an I/O operation.
  We call concurrent execution contexts **fibers**.
- Parallelism: having multiple execution contexts run at the same time.  
  Examples are **threads** and **domains** (an [OCaml 5 term](https://v2.ocaml.org/manual/parallelism.html)).
  Parallelism is often used for CPU-bound workloads as parallel execution contexts run on multiple cores.
- Preemptive vs cooperative:

  - In a preemptive system, a scheduler can at any time switch to a difference execution context.
  - In a cooperative system, the execution contexts explicitly relinquish control to switch to another execution context.

  In practice, I think parallelism is always preemptive and concurrency mostly cooperative.

- [OCaml fibers](https://v2.ocaml.org/manual/effects.html#s:effects-fibers) vs [Eio fibers](https://ocaml-multicore.github.io/eio/eio/Eio/Fiber/index.html)
  Both OCaml 5 and Eio have a concept of fiber, which are related but not the same.

  - For Eio, a fiber is a concurrent execution context, which can be spawned, awaited, cancelled, etc.
  - For OCaml 5, fibers are "language-level threads implemented
    using runtime support for heap-allocated, dynamically resized, stack segments".
    The OCaml runtime stack is a list of fibers and when entering an effect handler a new fiber is allocated and appended.
    When performing an effect the continuation passed to the effect handler then contains all fibers (i.e. all stack frames) between the point where the effect is performed and the handler that actually handles it.

  So my interpretation is that all Eio fibers are OCaml fibers (because each Eio fiber creates its own effect handler to handle `Fork` and `Suspend` effects, which is what enables the concurrency of Eio fibers) but Eio adds a higher-level API.

  We will mostly talk about Eio fibers, so we just call them **fiber**.
  When talking about multi-threading, we use **domains** or **threads** interchangeably.

## Features of Eio

As a structured concurrency library using OCaml 5 effects for "light-weight threads" (i.e. fibers), Eio offers the following features.

- Backends for multiple operating systems (Windows, Linux & general POSIX)
- Operations to manage resources (file descriptors, sockets, etc.) using the `Switch` module.
  Resource creation functions have a `Switch.t` argument where the resource is registered. When the switch is exited, all attached resources can be closed.
- Operations to spawn (`fork`, `fork_promise`, `fork_daemon`), cancel and await fibers.
- Multi-threading support
  - Each scheduler only handles [fibers in one domain](https://github.com/ocaml-multicore/eio/blob/286a1b743d3a55c5318a1301083e311f5b7d5b91/lib_eio_posix/sched.mli#L3). If you want fibers in multiple domains, each domain needs its own scheduler.
  - Fibers can await each other cross-domain. This is enabled by implementing promises using a multi-threaded lock-free queue (CQS).
- Abstractions for different operating system services (file system, networking, processes) and synchronization primitives like mutexes, semaphores & conditions

[Some more notes on the core modules of Eio](./eio-detail.md).

## What can we verify about the different features of Eio

Verifying the whole Eio library is a too wide scope and even the core library is with ~1300 LoC probably too big.
In the following we give more details on some of Eio's features, what we could be able to verify about them, and how we should simplify them.

### Fibers

There are many ways to create fibers in Eio: `Fiber.{fork, fork_promise, fork_daemon, all, both, pair, any, first}`

`fork_promise` spawns a new fiber and creates a promise to hold the final result. `fork` does not create a promise, so it is just fire-and-forget. `fork_daemon` is for background fibers with different cancellation behavior.

`all, both, pair, any, first` are combinators to run multiple fibers and aggregate their results.
Only `any` is nontrivial (a.d. TODO I still have not understood this function. The cancellation handling seems quite complicated.)

From the above mentioned featues, we probably only want to keep the basic `Fiber.fork_promise` (and maybe `Fiber.any`) because others are just a basic variation on it.
`fork_promise` returns a promise that will later be fulfilled by a value, so it should satisfy a specification like:

```
{{ P }} EWP e () <| Coop |> {{v, Q v}} -*
  {{ P }} EWP fork_promise e {{p, is_promise p Q}}
```

Also, fibers support thread-local state that can be queried by an effect.
This could be modelled by protocols with a ghost-cell parameter to describe the variables. (a.d. TODO more explanation why the parameter is needed. In short, so that both handler and handlee know from the beginning what data is transferred in the effect.)

### Multi-threading

Since many safety concerns in Eio arise from absence of data races, we want to support multi-threading.
Thread-safety concerns and how multi-threading is used in Eio is detailed [here](./eio-thread-safety.md)

Hazel has no multi-threading support and only basic support for invariants. For verifying the thread-safety of functions we need both. We describe how to use invariants [here](./hazel-invariants.md)

Adding multi-threading should be straight-forward, but a lot of work, so it has not been a priority so far.
Effects cannot be performed cross-thread, so spawning a new thread should obey the empty protocol.
Otherwise, multiple threads should work the same as in heap-lang.

### Switches & Cancellation Contexts

For each domain, there is a tree of **cancellation contexts** and each fiber belongs to one.

Cancelling the context cancels all attached fibers (and those of unprotected child contexts).
Right after being scheduled, and before calling any resource APIs a fiber checks if it was cancelled.

A `Switch.t` manages resources and fibers and is also attached to a cancellation context.
A new switch is created with `Switch.run (fn sw -> ...)`.
If a switch is cancelled, it cancels the attached cancellation context which in turn cancels the fibers.
Before `Switch.run` returns, it waits until the non-daemon fibers are completed and then cancels all remaining daemon fibers.

We describe our initial appraoch on how to model cancellation [here](./hazel-access-rights.md).
The idea was to verify that after a fiber has been cancelled, not resource API can be called again.
However, cancellation in Eio is pretty weak and acts only as a signal for a fiber to voluntarily shut down.
A misbehaved fiber can go into a protected context **after** it has been cancelled and continue called resource APIs.
So there is almost nothing to verify.
(a.d. TODO what might be interesting is all the cancellation hooks that are registered for various operations, like waiting on a promise)

Spawning new domains also interacts with cancellation, but rather ad-hoc.
The `Domain.run` function receives a cancellation promise which it awaits concurrently with the domain's main function.
If the spawning fiber is cancelled, the promise is resolved.
In that case, an exception is raised in the spawned domain.
(a.d. this behavior seems different than normal cancellation as fibers in the main thread can catch their own cancelled exceptions and continue running. But here the exception would directly go to the scheduler and terminate the thread. TODO test this)

### Resource usage

We do not want to axiomatize all possible resources that Eio offers.
Instead, if we want some justification to model a `Switch`, we could add some opaque, generic resource.

Then the goal would be to prove that after a `Switch` is over, none of the attached resources remain.

## Progress

We start with the `asynchronous_computation.v` case study from Hazel and interatively extend it to a (simplified) Eio case study.

Development on the case studies happens in the [hazel submodule](../hazel).

- [x] `asynchronous_computation_passing.v`  
       Small case study about verification of a small scheduler with cancellable fibers (turns out cancellation in Eio is less powerful so the verification does not apply to real Eio)
- [x] `asyncrhonous_computation_eio.v`  
       Medium case study about verification of a scheduler that uses `Fork` and `Suspend` effects like in Eio (but still much simplified compared to Eio).
      The CQS interface is still axiomatized.
      The proofs in this case study are described [here](./eio-light-case-study).
- [ ] Medium case study where we add thread-local variables and a verified CQS interface.
- [ ] Final case study about verification of a simplified Eio scheduler with
  - the Eio effects `Fork`, `Suspend` & `GetContext`
  - some generic OS resource interface to be managed by `Switch`
  - multi-threading
  - thread-local variables
  - an extended CQS
  - an example program
- [ ] Extending support for invariants in Hazel
  - [x] Define atomic expressions.
  - [ ] Add an invariant opening lemma to avoid doing `inv_access` manually.
- [ ] Extending Hazel operational semantics with multiple threads.
- [ ] Extending the CQS proof with the new functions and states that Eio adds.

## References

- [Retrofitting Effect Handlers onto OCaml](https://arxiv.org/abs/2104.00250)
- [Retrofitting Parallelism onto OCaml](https://arxiv.org/abs/2004.11663)
- [CQS: A Formally-Verified Framework for Fair and Abortable Synchronization](https://arxiv.org/abs/2111.12682) (a shorter journal version from 2023 also exists)
- TODO
