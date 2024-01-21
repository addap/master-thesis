# Verifying (a simplified version of) the Eio library

The goal of this thesis is to verify a simplified version of the core of the Eio library for OCaml 5.

The recently released OCaml 5 includes several new features like multi-threading support and an effect sytem.  
[Eio](https://github.com/ocaml-multicore/eio/) is a concurrency library for OCaml 5 that uses effects to implement fibers, which are cooperatively scheduled execution contexts.
Eio aims to be the standard concurrency library for OCaml 5.
[Hazel](https://gitlab.inria.fr/cambium/hazel) is a formalization of a language with effects in Iris.
Hazel has been used in multiple case studies, among them a cooperative scheduler implementing fibers with effects (but with a different implementation than Eio uses).

For server-like applications running many short-lived tasks, concurrent programming has gained in popularity as a way to organize the code and efficiently use I/O resources (e.g. goroutines in Golang and async/await in Rust).
If Eio does position itself as the standard concurrency library for OCaml 5, then it would be nice to have the reassurance of a formal verification of its core abstractions.

As such, we want to show the **safety** and **effect safety** of the core of Eio, which includes fibers for concurrent execution, promises for communication between fibers, and a scheduler based on Eio's mock scheduler. (Eio's mock scheduler is the common denominator between the specialized per-operating system scheduler implementations.)

## Structure

The rest of this document is organized as follows.

- Section 0 tracks the progress of this work as a quick summary.
- Section 1 defines some terms we use throughout.
- Section 2 discusses Eio in more detail.
- Section 3 shows how and what specifications we want to prove about Eio's API.
- Section 4 discusses the existing verification of CQS (explained later)
- Section 5 shows how we adapt this development to use it for verifying Eio.

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
- [x] We extend the Hazel operational semantics with multiple threads.
- [x] We adapt the CQS verification so that we can use it for the customized CQS variant that Eio defines.
  - [ ] Instead of manually translating the CQS program & proofs we could prove that a WP in heaplang implies an EWP in Hazel as explained [here](./cqs-proof-by-translation.md).
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

Eio is a structured concurrency library implemented using OCaml 5 effects.

Eio acts as an async runtime providing access to operating system resources (file system, network, timers, etc.) to code organized in fibers.
The fibers are cooperatively scheduled and many operations on Eio provided resources are suspension points (reading from a socket, awaiting a timer, etc.).
Fibers can spawn new fibers in the same thread and then cancel them or await their completion.

Compared to previous concurrency libraries like Lwt and Async (which have a monadic interface), concurrency based on effects is easier to use because it is more composable, i.e. you can compose concurrent code with any non-concurrent code.
In practice, this is because for monads all of the code between the monadic run function and the actual usage of the operations of the monad needs to be written in monadic style.
No such restriction exists for effects.

Eio offers the following features.

- Backends for multiple operating systems (Windows, Linux & general POSIX)
- Operations to manage resources (file descriptors, sockets, etc.) using the `Switch` module.
  Resource creation functions have a `Switch.t` argument so that the resource is registered to the provided switch. When the switch is exited, all registered resources can be closed.
- Operations to spawn (`Fiber.fork`, `Fiber.fork_promise`, `Fiber.fork_daemon`), cancel (`Cancel.cancel`) and await (`Promise.await`) fibers.
- Multi-threading support
  - Each scheduler only handles [fibers in one thread](https://github.com/ocaml-multicore/eio/blob/286a1b743d3a55c5318a1301083e311f5b7d5b91/lib_eio_posix/sched.mli#L3). If you want to run fibers in multiple threads, each thread needs its own scheduler.
  - Fibers can await each other cross-thread. This is enabled by implementing promises using a multi-threaded lock-free queue (CQS).
- Abstractions for different operating system resources (file system, network, timers, etc.)
- Synchronization primitives like mutexes, semaphores & conditions.

[Some more notes on the core modules of Eio](./eio-detail.md).

## 3. How to verify Eio

For now OCaml 5 programs are not ensured to be effect safe.
In the future it is planned to define an effects system (analogous to a type system) for OCaml 5, which statically analyses what effects a program can perform.
With such an effects system it is then possible to statically ensure that a program does not perform any unhandled effects.  
In the meantime, we can use Hazel to manually prove the effect safety of a given program $e$.
We do this by proving an **extended weakest precondition** $\mathit{ewp}\; e\; \langle P\rangle\; \{x. Q\}$, which says that if e terminates in a value $x$, it satisfies $Q$ and the protocol $P$ can be performed during the execution.
A protocol is a collection of multiple effects.

To prove the effect safety of the Eio scheduler $\mathtt{run}$ for any given $\mathtt{main}$ function we thus want to prove an ewp with the empty protocol: $$\mathit{ewp}\; (\mathtt{run}\; \mathtt{main})\; \langle \bot\rangle\; \{\_. \top\}$$
Programs using Eio can mainly perform three effects: `Fork` to spawn a new fiber, `Suspend` to suspend and wait for some condition, and `GetContext` to get access to metadata about the current fiber.
The effects define the protocol `Coop`, so for any program using Eio, we want to prove an ewp with this protocol: $$\mathit{ewp}\; \mathtt{e}\; \langle Coop \rangle\; \{x. Q\}$$

Verifying the whole Eio library is a too wide scope and even the core library is with ~1300 LoC probably too big (see [eio-detail.md](./eio-detail.md)).
In the following we give more details on what we want to verify about Eio's core features.

### Fibers

There are many ways to create fibers in Eio: `Fiber.{fork, fork_promise, fork_daemon, all, both, pair, any, first}`.

`fork_promise` spawns a new fiber and creates a promise to hold the final result. `fork` does not create a promise, so it is just fire-and-forget. `fork_daemon` is for background fibers with different cancellation behavior.  
`all, both, pair, any, first` are combinators to run multiple fibers and aggregate their results.
Only `any` is nontrivial since it cancels other fibers as soon as one has completed.

From the above mentioned featues, we probably only want to verify the basic `Fiber.fork_promise` (and maybe `Fiber.any`) because others are just a basic variation on it.
`(fork_promise e)` returns a promise `p` that will later be fulfilled by the final value of `e`, so it has to satisfy a specification like the following, where `is_promise` says that the promise `p` will be fulfilled by a value satisfying `Q`.

```
{ P } EWP e () <| Coop |> {v, Q v} -*
  { P } EWP (fork_promise e) <| Coop |> {p, is_promise p Q }
```

The corresponding operation to await a promise then must satisfy the following specifiation.
Promises are built on top of CQS, which is explained separately below.

```
{ is_promise p Q } EWP (await p) <| Coop |> {v, Q v}
```

Also, fibers support thread-local state that can be queried by the `GetContext` effect. This is explained further [here](./eio-fiber-state.md).

### Multi-Threading

Some safety concerns only arise because Eio supports multiple threads, otherwise CQS would be unneeded and promises could be a lot simpler.
Therefore, we also want to support multi-threading in our case study.
Thread-safety concerns and how multi-threading is used in Eio is detailed in [eio-thread-safety.md](./eio-thread-safety.md).

Hazel has no multi-threading support and only basic support for invariants.
To verify the thread-safety of functions we need both.
We describe how to use invariants in [hazel-invariants.md](./hazel-invariants.md).

Hazel with multi-threading support could be achieved in two ways.
First, heaplang already has support for multi-threading so we could add support for effects to heaplang and the standard Iris program logic.
This would entail changing the language, operational semantics and the definition of WP.
But the existence of other features like prophecy variables and later credits complicate the matter as they could interact with effects so we decided against this approach.

Instead, we decided to change the Hazel language.
This is described in more detail in [hazel-multithreading.md](./hazel-multithreading.md).

### Cancellation Contexts

Fibers in Eio can be cancelled to give them a nonbinding signal to stop running.
This is implemented by assigning each fiber to a **cancellation contexts**.
Cancellation contexts are organized as a tree and cancelling one context has the effect of logically cancelling all attached fibers, and also cancelling its child contexts.
Right after being scheduled and before calling any resource APIs a fiber checks if it is cancelled, and if so, raises an exception.

Spawning new domains also interacts with cancellation.
The `Domain.run` function receives a cancellation promise which it awaits concurrently with the domain's main fiber.
If the spawning fiber is cancelled, the promise is resolved.
In that case, an exception is raised in the spawned domain.
This behavior seems different than normal cancellation as fibers in the main thread can catch their own cancelled exceptions and continue running.
But here the exception would directly go to the scheduler and terminate the thread. (TODO test if that is correct and maybe ask Eio people why this is intended behavior.)

Cancellation is supposed to stop fibers from doing any useful work but fibers can also be attached to a protected cancellation context.
Protected contexts and their fibers are unaffected by cancellation.
A fiber can even attach itself to a protected cancellation context after being cancelled.
So effectively, cancellation does not provide any guarantees about the behavior of fibers.
While cancellation, maintaining the tree of cancellation contexts, and the routing of cancellation exceptions throughout the program represents a good part of the complexity of Eio, we were not able to formulate any kind of specification for it.
This is why we decided to not model cancellation in our verification work.

It would still be interesting to show that cancellation does not introduce any unwanted crashes in Eio due to unhandled exceptions.
But as mentioned above, cancellation introduces a lot of extra code, which exceeds our time budget.
We could target it for future work (and maybe even reformulate cancellation so that we can give it a real specification).

Before we understood cancellation better we tried to give a specification for it, which is described [here](./hazel-access-rights.md).

### Switches

A `Switch.t` manages resources of fibers and is also attached to a cancellation context.
If a switch is failed, it cancels the attached cancellation context which in turn cancels the fibers.
Before `Switch.run` returns, it waits until all non-daemon fibers are completed and then cancels all remaining daemon fibers.

Since the two main functions of switches are controlling cancellation behavior (which we don't model) and cleaning up resources (a liveness property) we decided to also not model switches.
Instead, fibers are just run individually without attaching them to a switch.

### Resource usage

We do not want to axiomatize all possible resources that Eio offers.
Instead, if we want a justification for future work to model a switch, we could add some opaque, generic resource.
Then it could be possible to verify the cleanup behavior of switches using a similar approach as the the Iron framework does.

## 4. CQS

## 5. How we adapt CQS

## References

- [Retrofitting Effect Handlers onto OCaml](https://arxiv.org/abs/2104.00250)
- [Retrofitting Parallelism onto OCaml](https://arxiv.org/abs/2004.11663)
- [CQS: A Formally-Verified Framework for Fair and Abortable Synchronization](https://arxiv.org/abs/2111.12682) (a shorter journal version from 2023 also exists)
