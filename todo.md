### Cleanups TODOs

- [ ] clean up the code, especially
  - [ ] the case study
  - [ ] adapted cqs proofs (there are like 20 TODOs in the code)
- [ ] implement resume_all exactly as Eio does it.
- [ ] If porting CQS to Hazel proves too difficult, add effects to Heaplang.  
      [x] The problem is that CQS uses logical atomicity which I would also need to port to Hazel first.
      [x] That combined with having to add proper support for the iInv tactic (i.e. lots of engineering which is technically uninteresting but crucial) might actually make it easier to just add effects to Heaplang.
- [x] Alternatively, we could try to prove a lemma that allows us to transport a WP from heaplang to an EWP with the empty protocol.
- [ ] Can we use Iris' meta_token for promise_state_done γ
- [ ] Use the PR for better effect handling syntax in OCaml to check the examples I give in the thesis.
- [ ] try out completely chaning tlv dict
- [ ] remove future tense where it is unnecessary
- [ ] rename logical state to just ghost state.
- [ ] Cleanup CQS_Outer_Atomic
- [ ] Cleanup Cell_States

# Misc
- use devcontainer and latex-in-docker instead of installing latex & pygments on host