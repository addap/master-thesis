# What to focus on?

We want to know the dependencies between modules to see what we could feasibly
focus on. We create the dependency visualization with the following, and then
edit it manually.

```
codept lib_eio/*.ml lib_eio/core/*.ml -modules -dot | dot -Tpng > codept.png
```

<!-- ![Module Dependencies](./codept.png) -->

![Module Dependencies](./codept_fiber.png)

We can see that even if we just focus on the `Fiber` module we already have
dependencies on a lot of other modules. Some of them might
[change before the 1.0 release](https://github.com/ocaml-multicore/eio/issues/388).

| Module        | Use                                                                                                                                                                                                                                                                            | Stable?                                                 |
| ------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ | ------------------------------------------------------- |
| Fiber         | Defines the Fork effect to start a new fiber and several variations of the fork function (normal fork, fork as daemon, fork as promise, etc.)                                                                                                                                  |
| Switch        | A context manager for fiber resources. Has some convenience methods for running fibers in new or existing cancellation contexts.                                                                                                                                               | maybe                                                   |
| Exn           | Various exceptions used in eio.                                                                                                                                                                                                                                                |
| Single_waiter | Version of `Waiters` where only a single fiber can be waiting. (If another fiber would try to wait, the first one is forgotten and never resumed.)                                                                                                                             |
| Cancel        | The hierarchy of cancellation contexts to cancel fibers. Each fiber is associated with one cancellation context. Fibers will regularly check if they are cancelled and then raise an exception (e.g. when resuming from a yield, or when doing an operation that would block). |
| Suspend       | Performing the Suspend effect which should make a scheduler suspend the current fiber. The suspend effect carries the function that gets the fiber context and `enqueue` function to arrange for the fiber to be enqueued in the run-queue again.                              |
| Promise       | An implementation of promises that can be awaited and resolved. (Not sure why they use the Obj.magic, variance and injectivity type attributes) Internally, it uses broadcast to be able to keep a list of waiting fibers and wake them up at once.                            |
| Waiters       | Keeps a list of suspended fibers that are waiting for an event. The standard case seems to be that await uses the Suspend effect and then calls the enqueue function provided by the scheduler when the fiber should be woken up.                                              | [no](https://github.com/ocaml-multicore/eio/issues/382) |
| Hook          | Used if a fiber that is in the waiting list is cancelled and the waiting list is used in multiple domains. The hook then reaquires the mutex before removing the cancelled fiber from the list.                                                                                |
| Broadcast     | Keeps a list of suspended fibers that are waiting for an event. The difference to `Waiters` seems to be that in a `Broadcast` all have to be awoken at the same time and individual fibers can be cancelled.                                                                   | recently created                                        |
| Cells         |                                                                                                                                                                                                                                                                                |
| Ctf           | Unimportant. It is is just used for tracing.                                                                                                                                                                                                                                   |

In total these have about 1300 lines of code.

```
$ cloc lib_eio/core/{fiber.ml,switch.ml,exn.ml,single_waiter.ml,cancel.ml,suspend.ml,promise.ml,waiters.ml,hook.ml,broadcast.ml,cells.ml}
-------------------------------------------------------------------------------
Language                     files          blank        comment           code
-------------------------------------------------------------------------------
OCaml                           11            273            238           1285
```

So, we probably have to do a finer dependency analysis to cut down the scope.
How hard are the dependencies between modules?

### Fiber

- Promise: just for run_promise, but that seems to be the only way a fiber can
  directly wait for another fiber (you could probably also write something
  yourself using mutexes and wait).
- Switch: inherent
- Suspend: inherent
- Single_waiter: Weak, Seems to be only used for the "concurrent list
  operations" inner module.

### Promise

Hard dependencies on Suspend, Cancel & Broadcast

### Switch

Needs Cancel but Waiters might be replaced by Single_waiter.

# General Questions

## Functions that must deal with being called in restricted contexts

There are some informal constraints on functions like "must be able to be called
in any context". E.g. in `broadcast.mli`.

```
[fn ()] may be called from the caller's context, or by [resume_all],
so it needs to be able to cope with running in any context where that
can run. For example, [fn] must be safe to call from a signal handler
if [resume_all] can be called from one. [fn] must not raise.
```

In practice, you must not call any blocking or suspending code from a signal
handler.

I guess you can add ghost state to model something like from what kind of
context a function was called in. e.g. we assume a signal handler axiom. And we
also have the `NormalCtx` which is passed to the main thread. Some function that
must not be called from a signal handler then requires a `NormalCtx`.

```
Sys.set_signal : int -> (int -> unit) -> unit

SigHdlAxiom : { SigHdlCtx } hdl { True } -* { True } Sys.set_signal s hdl { True }
```
