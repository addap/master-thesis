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

# 1. Introduction

- As a motivation for the work: program verification, safety and why we care about it.
- Iris and how we can prove safety
- Eio and how it provides concurrency primitives
- Effect Handlers with a simple example
- Effect Handler formalization in Iris using protocols

## Simple Scheduler

- The simplified code of the mock scheduler that Eio provides (actual schedulers differ per OS and intergrate with OS primitives)
- What is the difference between this scheduler and the one from Paolo's paper  
  The gist is that using a concurrent queue and handling promises in the fibers allows many simplifications in the scheduler & the logical state.
- Explain how the Fork/Suspend effect work and how fibers use them.
- Explain how the scheduler implements these effects.
- What are the safety concerns in this implementation (mainly in the implementation for await)
- What logical state in Iris do we use to model the behavior.
- What are some interesting parts of the proofs.

## Towards A Multi-Threaded Scheduler

### Adding Invariants to Hazel

- [short] How are invariants used and implemented in Iris.
- What does Hazel already provide for invariants and what is missing.
- How we prove atomicity for the basic operations.
- [tbd] How we add support for the iInv tactic to use invariants more easily.

### Adding Multi-Threading to Hazel

- [short] How is multi-threading implemented in Heaplang.
- We use the same approach and extend the definition of Hazel.
- What are the interesting points in the implementation.  
  We don't have a frame for `Fork`, one of the proofs was a little bit tricky but everything else was standard.

## Verifying an Adaptation of CQS

- What is CQS used for
- What is the logical state used for the verification
- How we adapt CQS (use promises & and resume_all)
- How we change the logical state to match our adapted version.

## Extending the Scheduler with Thread-Local Variables

- How thread-local variables can be used.
- Explain the GetContext effect in Eio and how we model it in our scheduler.
- How we adapt our logical state to include GetContext.
  And explain that we need to parameterize the protocol to solve the issue of shared knowledge between the scheduler and fiber.

## A Note on Cancellation

- That we tried to model cancellation but the feature is too permissive to give it a specification.
- There is still an interesting question of safety (fibers cannot be added to a cancelled Switch).  
  But including switches & cancellation in our model would entail too much work so we leave it for future work.

## Evaluation

## Conclusion
