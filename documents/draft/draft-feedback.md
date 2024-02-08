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

# 2024-02-05

- [x] merge section 3 with section 2  
  Since the basic scheduler is already multithreaded I should just take multithreaded Hazel as a given and note in the appendix what I needed to add to Hazel
  Just mention that we had to change Hazel in the introduction
- [x] briefly say that effect handlers don't ineract with multithreading. 
  We can readon about programss that have both (introduction)
- [ ] Improve the description of CQS.
  - [ ] When explaining the await function in chapter 2, briefly describe what service it provides and that the implementation is due to it being lock-free. 
    Also here we can explain why the cancel function is necessary.
  - [ ] Introduction of CQS respective suspend & cancel operation are not understandable. 
    Since we explain the await function before we can delete a lot of it and not talk about "handles" at all, just use callbacks directly.
- [x] choose different names for CQS functions (maybe just enqueue dequeue) (maybe register-signal_all [subject-observer system])
  I think the condition variable is the closest match
  - https://en.wikipedia.org/wiki/Observer_pattern
  - https://en.wikipedia.org/wiki/Monitor_(synchronization)#Condition_variables
- [x] call parameter of suspend effect waker
- can use the original CQS paper state diagram as long as I credit it




