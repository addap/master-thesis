### Cleanups TODOs

- [ ] clean up the code, especially
  - [ ] the case study
  - [ ] adapted cqs proofs (there are like 20 TODOs in the code)
- [ ] implement resume_all exactly as Eio does it.
- [ ] If porting CQS to Hazel proves too difficult, add effects to Heaplang.  
       The problem is that CQS uses logical atomicity which I would also need to port to Hazel first.
      That combined with having to add proper support for the iInv tactic (i.e. lots of engineering which is technically uninteresting but crucial) might actually make it easier to just add effects to Heaplang.
- [ ] Alternatively, we could try to prove a lemma that allows us to transport a WP from heaplang to an EWP with the empty protocol.
