# How do we use Invariants in Hazel

Hazel already has most of the infrastructure needed to support invariants.
In [`state_interpretation.v`](../hazel/theories/program_logic/state_interpretation.v) a global ghost cell to hold the set of invariants is defined.
Also, [`basic_reasoning_rules.v`](../hazel/theories/program_logic/basic_reasoning_rules.v) defines the basic `ewp_atomic` lemma, which enables changing a mask around a `StronglyAtomic` atomic expression.

But it does not define which expressions are atomic.
We do this in the file [`atomics.v`](../hazel/theories/case_studies/eio/atomics.v).
This works the same as in heaplang: Atomic expressions are defined as expressions that reduce to a value in a single step, which is proved by inversion on the reduction relation.
At the moment this is part of our scheduler case study but it should move into the program_logic directory in the future.

## TODO

### Add automation

We need to define an invariant opening lemma and somehow make the `iInv` tactic use it.
So far I am opening & closing invariants and managing view shifts manually with `ewp_atomic`, `inv_acc`, and `fupd_trans_frame`.

### Performing an effect as an atomic operation

Paulo mentioned an idea for invariants that can be opened over performing an effect.
Since the operational semantics of effects involve many reduction steps (building up the continuation + whatever the handler does), normal invariant access does not work.
But it could be possible to do something similar to logically atomic triples, i.e. logically atomic protocols.

My current assumption is that if the handler function is logically atomic

$$
    \langle x. P \rangle h \langle Q \rangle
$$

then we could define a logically atomic protocol by extension, which can only be used with a logically atomic handler.

$$
    ! x (v) \langle P \rangle ? y (w) \langle Q \rangle
$$
