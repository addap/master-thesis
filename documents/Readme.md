# Verifying (a simplified version of) the Eio library

The goal of this thesis is to verify a simplified version of the core of the Eio library for OCaml 5.

The recently released OCaml 5 includes several new features like multi-threading support and an effect sytem.  
[Eio](https://github.com/ocaml-multicore/eio/) is a concurrency library for OCaml 5 that uses the effect system to implement fibers, which are cooperatively scheduled execution contexts.
Eio aims to be the standard concurrency library for OCaml 5.
[Hazel](https://gitlab.inria.fr/cambium/hazel) is a formalization of an effect system in Iris.
It has been used in multiple case studies, among them a cooperative scheduler implementing fibers with effects (but with a different implementation than Eio uses).

For server-like applications running many short-lived tasks, concurrent programming has gained in popularity as a way to organize the code and efficiently use I/O resources (e.g. goroutines in Golang and async/await in Rust).
If Eio does position itself as the standard concurrency library for OCaml 5, then it would be nice to have the reassurance of a formal verification of its core abstractions.

As such, we want to show the **safety** and **effect safety** of the core of Eio, which includes fibers for concurrent execution, promises for communication between fibers, and a scheduler based on Eio's mock scheduler. (Eio's mock scheduler is the common denominator between the specialized per-operating system scheduler implementations.)

## Structure

The rest of this document is organized as follows. Some parts might link to other documents so that this one does not get too large.  
Section 0 tracks the progress of this work as a quick summary.
Section 1 defines some terms we use throughout.
Section 2 discusses Eio in more detail and section 3 shows how and what specifications we want to prove about Eio's API.
Section 4 discusses the existing verification of CQS (explained later) and section 5 shows how we adapt this development to use it for verifying Eio.

## 0. Progress

Hazel already contains a case study dealing with a cooperative scheduler (`asynchronous_computation.v`).
We used this as a basis for our work and iteratively adapted and extended it to serve as a case study on Eio's mock scheduler.

Development happens mostly in the [hazel submodule](../hazel).

- [x] [`asynchronous_computation_eio_cancel.v`](../hazel/theories/case_studies/asynchronous_computation_eio_cancel.v)  
       We explore a possible verification of a small scheduler with cancellable fibers. As explained in the section on Eio, cancellation is more powerful than we thought which prohibits any kind of specification for it.
- [x] [`asynchronous_computation_eio_getcontext.v`](../hazel/theories/case_studies/asynchronous_computation_eio_getcontext.v)  
       We explore how to verify a small scheduler that handles Eio's `GetContext` effect. This effect gives a fiber access to some of its metadata.
- [x] [`eio/`](../hazel/theories/case_studies/eio/)  
       We verify a scheduler using Eio's `Fork` and `Suspend` effects.
      The CQS interface is assumed as axioms, but it's verified separately using heaplang under the assumption that the proofs carry over as soon as we extended hazel with multi-threading.
- [ ] We extend the support for using invariants in Hazel.
  - [x] Define atomic expressions.
  - [ ] Add an invariant opening lemma to avoid doing `inv_access` manually.
- [ ] We extend the Hazel operational semantics with multiple threads.
- [x] We adapt the CQS verification so that we can use it for the customized CQS variant that Eio defines.
- [ ] All developments mentioned above need to be combined to a final case study where we verify a scheduler with
  - the Eio effects `Fork`, `Suspend` & `GetContext`
  - multi-threading
  - thread-local variables
  - an extended CQS
  - an example program

## 1. Definitions

- **Safety**: A safe program does not crash.
- **Effect safety**: An effect safe program does not perform any unhandled effects.  
  In OCaml 5, an unhandled effect crashes the program so technically effect safety is necessary for safety but we call it out separately.
- **Execution context**: General term for something that executes code with some environment.
- **Concurrency**: having multiple execution contexts run concurrently.  
  Examples are async/await, "green threads", "light-weight threads", coroutines, etc.
  Concurrent execution contexts do not necessarily run at the same time.
  Instead, concurrency is a way to structure a program which executes multiple functions to switch to a different one if an execution context gets blocked.
  Concurrency is often used for I/O-bound workloads to run a different execution context while another one is blocked waiting for the completion of an I/O operation.
  In Eio, concurrent execution contexts are called **fibers**.
- **Parallelism**: having multiple execution contexts run at the same time.  
  Examples are **threads** and **domains** (an [OCaml 5 term](https://v2.ocaml.org/manual/parallelism.html)).
  Parallelism is often used for CPU-bound workloads as parallel execution contexts run on multiple cores.
- **Preemptive** vs. **cooperative**:

  - In a preemptive system, a scheduler can at any time switch to a difference execution context.
  - In a cooperative system, the execution contexts explicitly relinquish control to switch to another execution context.

  In practice, I think thread-based parallelism is always preemptive and concurrency is mostly cooperative.

### [OCaml fibers](https://v2.ocaml.org/manual/effects.html#s:effects-fibers) vs [Eio fibers](https://ocaml-multicore.github.io/eio/eio/Eio/Fiber/index.html)

Both OCaml 5 and Eio have a concept of fiber, which are related but not the same.

- For Eio, a fiber is a concurrent execution context, which can be spawned, awaited & cancelled.
- For OCaml 5, fibers are "heap-allocated, dynamically resized stack segments" as the runtime stack consists of a list of fibers.  
  Ocaml 5 continuations are also modeled as a list of fibers.
  When entering an effect handler, a new fiber is allocated and appended to the stack.
  When performing an effect E, the suffix of the stack beginning at the fiber of the handler handling E is removed from the stack and is now called a continuation.
  This continuation is passed to the handler.
  Invoking a continuation then appends its list of fibers to the current stack.

So my interpretation is that each Eio fiber has an associated OCaml fiber, because a new effect handler is entered when an Eio fiber is spawned.
Capturing OCaml fibers in the form of continuations and invoking them at a later time is what enables the concurrent behavior of Eio fibers.

We will mostly talk about Eio fibers, so we just call them **fiber**.
When talking about multi-threading, we use **domains** or **threads** interchangeably.

## 2. Eio

Eio is a structured concurrency library using OCaml 5 effects for "light-weight threads" (i.e. fibers).  
Compared to previous concurrency libraries like Lwt and Async (which have a monadic interface), concurrency based on effects is easier to use because it is more composable, i.e. you can compose concurrent code with any non-concurrent code.
In practice, this is because for monads, everything between the monadic run function and the code actually using the operations of your monad needs to be written in monadic style. No such restriction exists for effects.  
However, for now OCaml 5 programs are not ensured to be effect safe.

Eio offers the following features.

- Backends for multiple operating systems (Windows, Linux & general POSIX)
- Operations to manage resources (file descriptors, sockets, etc.) using the `Switch` module.
  Resource creation functions have a `Switch.t` argument where the resource is registered. When the switch is exited, all attached resources can be closed.
- Operations to spawn (`fork`, `fork_promise`, `fork_daemon`), cancel and await fibers.
- Multi-threading support
  - Each scheduler only handles [fibers in one domain](https://github.com/ocaml-multicore/eio/blob/286a1b743d3a55c5318a1301083e311f5b7d5b91/lib_eio_posix/sched.mli#L3). If you want to run fibers in multiple threads, each thread needs its own scheduler.
  - Fibers can await each other cross-thread. This is enabled by implementing promises using a multi-threaded lock-free queue (CQS).
- Abstractions for different operating system services (file system, networking, processes) and synchronization primitives like mutexes, semaphores & conditions

[Some more notes on the core modules of Eio](./eio-detail.md).

## 3. How to verify Eio

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

Since some safety concerns only arise because Eio supports multiple threads (otherwise CQS would be unneeded, and promises could be a lot simpler), we also want to support multi-threading in our case study.
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

## 4. CQS

## 5. How we adapt CQS

## References

- [Retrofitting Effect Handlers onto OCaml](https://arxiv.org/abs/2104.00250)
- [Retrofitting Parallelism onto OCaml](https://arxiv.org/abs/2004.11663)
- [CQS: A Formally-Verified Framework for Fair and Abortable Synchronization](https://arxiv.org/abs/2111.12682) (a shorter journal version from 2023 also exists)
- TODO
