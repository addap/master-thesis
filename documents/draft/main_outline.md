# Verifying an Effect-Based Cooperative Concurrency Scheduler in Iris

## Sections

1. Introduction
2. Simple Scheduler (with axiomatized CQS)
3. Towards a Multi-Threaded Scheduler
4. Using Multi-Threading to verify an Adaptation of CQS
5. Extending the Scheduler with Thread-Local variables
6. A Note on Cancellation
7. Evaluation
8. Conclusion
9. Bibliography

## 1. Introduction

- Program verification, safety and why we care about it.
- Iris and how we can prove safety
- As a motivation: Eio wants to become the standard concurrency library.  
  We want to verify the safety of the central abstractions.
- Effect handlers with a simple example
- Effect handler formalization in Iris using Hazel

## 2. Simple Scheduler

- Show the simplified code of the mock scheduler that Eio provides (actual schedulers differ per OS and intergrate with OS primitives)
- What is the difference between this scheduler and the one from Paulo's paper  
  The gist is that using a concurrent queue and handling promises in the fibers allows simplifications in the scheduler code & the logical state.
- Explain how the Fork/Suspend effect work and how fibers use them.
- Explain how the scheduler implements these effects.
- What are the safety concerns in this implementation (mainly in the implementation for await)
- What logical state in Iris do we use to model the behavior.
- What are some interesting parts of the proofs.

## 3. Towards A Multi-Threaded Scheduler

- OCaml supports multi-threading and Eio implements spawning a scheduler in a new thread.
- That's why we also want to incorporate it in our model.
- But we first need to add support for invariants and a multi-threaded semantics to Hazel.

### 3.1. Adding Invariants to Hazel

- [short] How are invariants used and implemented in Iris.
- What does Hazel already provide for invariants and what is missing.
- How we prove atomicity for the basic operations.
- [tbd] How we add support for the iInv tactic to use invariants more easily.

### 3.2. Adding Multi-Threading to Hazel

- [short] How is multi-threading implemented in Heaplang.
- We use the same approach and extend the definition of Hazel.
- What are the interesting points in the implementation.  
  We don't have a frame for `Fork`, one of the proofs to use Iris' language interface was a little bit tricky but everything else was standard. Also, empty protocol on `Fork`.

## 4. Verifying an Adaptation of CQS

- What is CQS used for
  Lock-free synchronization primitive. Enables a number of threads to wait for an event.
- [short] What is the logical state used for the original verification.  
  CQS supports many features so the map of possible states is quite complicated.
- How we adapt & simplify CQS  
  We use promises, add resume_all & do not use many features.
- How we need change the logical state to match our adapted version.
- The complication that arises from a resume_all operation and the fact that we do not count the number of waiters anywhere.

## 5. Extending the Scheduler with Thread-Local Variables

- Usage scenario of thread-local variables.  
  e.g. a shared log
- Explain the GetContext effect in Eio and how we model it in our scheduler.
- How we adapt our logical state to include GetContext.
  And explain that we need to parameterize the protocol to solve the issue of shared knowledge between the scheduler and fiber.

## 6. A Note on Cancellation

- That we tried to model cancellation but the feature is too permissive to give it a specification.
- There is still an interesting question of safety (fibers must be added to a cancelled Switch).  
  But including switches & cancellation in our model would entail too much work so we leave it for future work.

## 7. Evaluation

## 8. Conclusion
