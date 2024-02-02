# 2024-01-29

- [x] give spec of create operation
- [x] focus more on the fact that there is some outer datastructure, it handles stopping/resuming and CQS is just a store for callbacks
- [x] diagram of how CQS is supposed to be used (with an outer atomic variable)
- [x] give OCaml types of CQS functions, write a little about it
- [x] comparison with original CQS should come at the end. Explain the diagram of the cell transition in more detail (use the commented out text) in combination with the OCaml functions & types
- [x] fix "must support stopping execution"
- [x] why is cancel supported (done when explaining promises in detail)?
- [x] rename `is_thread_queue`
- [x] for spec, explain which predicates are persistent
- [x] just use one gamma instead of 4/5 in operation spec
- [x] since resume_all permit is unique, queue becomes useless after resume all has been called.
- [x] fix math character issue in markdown
- [ ] rewrite in latex
