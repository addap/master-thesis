### Cleanups TODOs

- [x] If porting CQS to Hazel proves too difficult, add effects to Heaplang.  
      [x] The problem is that CQS uses logical atomicity which I would also need to port to Hazel first.
      [x] That combined with having to add proper support for the iInv tactic (i.e. lots of engineering which is technically uninteresting but crucial) might actually make it easier to just add effects to Heaplang.
- [x] Alternatively, we could try to prove a lemma that allows us to transport a WP from heaplang to an EWP with the empty protocol.
- [x] Can we use Iris' meta_token for promise_state_done γ
- [x] Use the PR for better effect handling syntax in OCaml to check the examples I give in the thesis.
- [x] try out completely changing tlv dict
- [x] remove future tense where it is unnecessary
- [x] rename logical state to just ghost state.
- [x] Cleanup CQS_Outer_Atomic
- [x] Cleanup Module hierarchy.
- [x] Cleanup Cell_States
- [x] Fix some parts of the hoare style proofs that are hard to understand.
- [x] Add a section on the deferred queue.
- [x] Capitalize figure captions consistently.
- [x] Actually prove the case study.
- [x] Cleanup duplicate info from section 2 (I explain the tryUnregister operation 3 times...)
- [x] Fix that I call it the "Hazel language", it should be the "HH language" and the "Hazel formalism".
- [x] Mostly replace quotation marks by italics
- [x] fix all grammatical errors shown by spellcheck
- [ ] Port deferred queue to eio by copying the new dfrac saved_prop implementation to my older Iris version.
- [x] fix missing argument of gspwaiting/gspdone
- [x] fix missing argument of proto/gsiswaker/etc.
- [x] make scheduler result non-optional
- [x] Move repo to mpi gitlab and then place link in pdf
- [x] Add missing citations

### Code cleanup

- [ ] clean up the code, especially
  - [ ] the case study
  - [ ] adapted cqs proofs (there are like 20 TODOs in the code)
- [ ] Organize the different Iris versions that I am using.
