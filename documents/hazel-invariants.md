# How do we use Invariants in Hazel

TODO, points to mention:

- Hazel by default only uses `StronglyAtomic` but I think we should be able to use `WeaklyAtomic` too.
- Hazel already has a ghost cell for all invariants built into the state interpretation, so basic support is there
- we need to define which expressions are Atomic (done for load, store, alloc)
- we need to define an invariant opening lemma to make it more convenient. So far I am opening & closing it manually with `ewp_atomic`, `inv_acc`, `fupd_trans_frame` and `iApply "Hclose"`
- Paulo mentioned an idea for invariants that can be opened over performing an effect.
