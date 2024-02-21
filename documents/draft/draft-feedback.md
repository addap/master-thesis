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

# 2024-02-15

- [x] Separate scheduler proof & spec from promise proof & spec.
  - [x] remove mention of promiseInv from Suspend case of coop protocol
- [ ] Separation between modules of OCaml code should be clearer
- [ ] Clarify which parts of Iris definitions are part of a module's API and which are internal
- [ ] In module hierarchy, explain what the arrows mean
- [x] page 5, "we discuss the implementation of the simplified Eio model in more
detail to give an intution about what their specification and the logical
state to prove them": not clear what "their" and "them" refers to.
- [x] page 5, the type of `Scheduler.run` suggests that a result is dropped.
This is OK but maybe a little weird?
- [x] page 5, "The promise is thread-safe so any other fiber can use [...]".
Maybe this has been said earlier, but you can clarify that the fibers
may be running on top of multiple hardware threads, and that it is OK
for a promise to be shared across threads.
- [x] Actually, it would be desirable to clarify whether the scheduler uses
one hardware thread or more than one hardware thread. In the code that
is shown, one does not see new hardware threads being spawned, so it
looks as if the scheduler uses only one thread.
- [x] If the scheduler uses multiple hardware threads then it would be desirable
to show how/when these threads are created, and to clarify whether each
hardware thread has its own run queue (I think this is the case?).
- [x] page 5, "the concrete syntax of effect handlers is verbose" : just FYI,
we should have nicer syntax in 5.3. Currently it exists in a PR,
https://github.com/ocaml/ocaml/pull/12309
(It may desirable to install this PR on your machine so you can syntax-check
and type-check the code that you show. Note that there is now a comma in the
effect-handling syntax: `match ... with effect E x, k -> ...`.)
- [x] top of page 6, it may be worth clarifying that this is a deep handler,
which means that the monitored expression `e` can perform the effect
`E` several times, and every time, the effect will be handled by this
handler.
- [x] `let run_queue := queue_create ()`
  should be `=`, not `:=`
- [x] page 6, the inner function `fork` could be named `loop` or `execute`.
- [x] page 6, there are two calls to `next run_queue`, but the definition
of `next` is not shown. It would be desirable to show it. I suppose
it extracts a ready thread out of the run queue and executes it?
(And if the run queue is ready, then it stops.) Also, I imagine that
it could be defined at the same level as `fork` (they are mutually
recursive?) so it does not need to take `run_queue` as an argument.
- [x] page 6, in the line `_ -> next run_queue`, it would be good to explain
why the result of the fiber is dropped. This may seem surprising.
I imagine that the fiber itself is responsible for updating a promise,
therefore the scheduler has nothing to do with the result?
(A forward pointer to the explanation of `fork_promise` could be given.)
Actually, now that I think of it, it would be desirable to clarify
what is the type of the functions stored in the run queue: is it
`unit -> unit`? If so, `_ -> next run_queue` should be replaced
with `() -> next run_queue`, which is clearer.
Same remark in other places: better use `()` instead of `_` wherever
possible, otherwise the reader wonders why some value is dropped.
- [x] page 6, in the two lines that follow `effect (Fork fiber) k`, you may wish to
explain that this scheduler pauses `k` and runs `fiber` immediately, but the
converse would also be valid. (In fact, it would also be valid to hand one of
these two fibers off to another hardware thread. Does the scheduler have any
way of balancing the load between hardware threads?)
- [x] page 6, "handling a Suspend effect may look complicated at first due to the
higher-order register function" : maybe explain that the `register` function
is constructed by the fiber that performs a `Suspend` effect, and that this
function is intended to be called immediately after this fiber has been
suspended (i.e., immediately after the continuation has been captured). And
this function receives as argument a "waker", or a "wakeup capability", which
it can store in a suitable place so that the fiber is woken up at a suitable
time in the future. [Part of the difficulty in explaining this setup is that
"the fiber who wants to be suspended" is indeed suspended as soon as the
`Suspend` effect takes place, yet it is still given an opportunity to run some
code, namely the `register` function.]
- [x] page 6, the line `effect (Suspend register)` is missing the binder `k`.
- [x] page 6, in `let waker = fun v ...`, it is difficult for the reader to figure
out what is the type of `v`. I suppose it is a locally quantified type `a`?
- [x] page 7, the function `Promise.create` and type `Promise.t` are not shown,
making the code difficult to read. One can infer that a promise is an atomic
reference to a sum `Done | Waiting`, but this should be shown.
- [x] page 7, "unreachable" -> "impossible" would sound more natural to me.
- [x] page 7, "This function is looks" (typo)
- [x] It may be preferable to show the pieces of OCaml code inside several figures
(one per OCaml module) and to explicitly indicate (in the caption) which
module is shown. Otherwise, the boundaries between modules are unclear.
- [x] page 7,  `let await (p: 'a Promise.t)` suggests that `await` is outside
the `Promise` module, but it is actually inside, right?
- [x] page 7, "The “suspend execution” part is handled by performing the Suspend
effect because the scheduler will switch to the next fiber in its run queue".
I don't really understand the end of the sentence ("because..."). I suggest
just removing it.
- [x] page 7, "Then, the “until p is fulfilled” part is handled by CQS". I see
what you mean, but the sentence may seem obscure to some readers. I would
say that CQS is used to register the fact that "when this promise is
fulfilled, this fiber should be woken up".
- [x] page 7, "functionally similar to condition variables in languages like C++
(only lock-free)". The analogy with condition variables is good. Note that
they have existed in C for a long time (they are part of the POSIX standard),
and they also exist e.g. in Java (notify/wait) or even in OCaml. The
parenthesis "(only lock-free)" is perhaps too terse: you could clarify that a
POSIX condition variable is always used in conjunction with a lock, whereas
here, we want something similar to a condition variable, but that is not
associated with a lock.
- [x] page 7, "Therefore, after the Suspend effect returns we know that the Done
branch is reached." The logical implication may be a little difficult to
follow. In a little more detail, IF from the point of view of the fiber who
performs a `Suspend` effect, the expression `perform (Suspend ...)` appears
to return, THEN the continuation must have been invoked, SO the waker must
have been called, SO the promise must have been fulfilled (calling a waker
before the promise has been fulfilled is forbidden).
- [x] page 7, "the register function" : the word `register` should be in teletype
font.
- [x] page 7, "the register function must take care of possible interleavings where
the promise is fulfilled in another thread". To clarify this, perhaps
emphasize again that the promise can be shared between multiple threads, so,
after we read the promise and find `Waiting`, the promise's state can be
changed to `Done` by another thread. Also, emphasize that while we are about
to call `CQS.register`, some other thread can call `CQS.signal_all`. In other
words, there is a race condition on the promise (the atomic cell) and a race
condition on the communication channel (the CQS).
- [x] page 7, "First, if CQS.register notices that there is a concurrent
CQS.signal_all it will directly call the waker. Otherwise, the waker is
registered but in fact the CQS.signal_all might have already finished before
CQS.register even started." One may wonder why these two cases are
distinguished. I mean, in the first case, where `register` notices that a
signal is taking place and calls `waker` directly, it would also be correct
for `register` to pretend that the signal took place earlier and could not
be noticed. Right? So, it would be correct (and simpler) for `register` to
never detect a signal and always return `Some _`. Right? So, the reason why
`register` sometimes returns `None` is just that this is an optimization
(it is more efficient). Am I correct?
- [x] page 7, "the callback" : the word "callback" may seem a bit obscure here;
I would suggest calling it a "waker" or (better?) a "wakeup capability"
or "wakeup function".
- [x] page 7, "try to cancel the callback registration and call it directly":
unclear what "it" refers to, and the sentence is generally unclear. I would
write something along the lines of: "check the state of the promise again,
and if the promise is now `Done` (which means that a signal has been sent
or will soon be sent), then cancel the registration of the wakeup function,
and invoke this function directly". [Finally, there remains to explain that
cancellation itself can fail, in case the signal has been received in the
meantime?, which is why we have `try_cancel` instead of `cancel`.]
- [x] page 7, I think it would help to show the file `CQS.mli` with comments.
Otherwise, the reader must guess the types of the CQS functions and what
they are supposed to do.
- [x] page 7, "The only safety concerns in the above implementations are
Fiber.fork_promise expecting the promise to be unfulfilled and Promise.await
expecting the promise to be fulfilled in the last match." You could clarify
what this means: we wish to prove that the two lines of the form `error
"unreachable"` are indeed unreachable (so these runtime errors will never
be observed).
- [x] page 8, "the usual ghost state constructs" may seem mysterious to a reader who
is not super-expert in Iris. Even I am not sure what ghost state you are
referring to. I imagine we need a ghost cell for each promise, to track its
logical state. What else?
- [x] page 8, "Fork stays almost the same" : you are implicitly making a comparison
with Paulo's protocol, but you do not show it. You should either show Paulo's
version and compare with yours, or show just yours and explain it from scratch,
without assuming the reader knows Paulo's version.
- [x] page 8, you cannot assume that the reader knows the concept of protocol and
the concrete syntax of protocols (involving !, ? and +). You should at least
explain (recall) that a protocol is roughly a mapping of each effect name to
a precondition/postcondition pair. And you could say that this is analogous
to an `effect` declaration in the OCaml code, which specifies the argument
type and result type of the effect.
- [x] page 8, "since promise handling is done entirely in the fibers, the scheduler
does not interact with the return value so the ewp degenerates to a trivial
postcondition". This cannot be understood by a non-expert. You need to help
the reader read the protocol. Basically the protocol states that in `perform
Fork e`, the value `e` must be a function of unit to anything, and this
function itself is allowed to rely on the protocol `Coop`, which means that it
can perform `Suspend` and `Fork` effects. In other words, the new fiber that
is being forked is allowed to call `await` and `fork`.
- [x] page 8, you should also explain the protocol for `Suspend`. I think that
`isRegister reg` should be `isRegister P reg`. Explain that `P` is the logical
condition under which waking up is permitted. (One could say that it is the
postcondition of the promise, but at this level of abstraction, there is no
notion of promise.) So `P` occurs in two places: 1- the waker can be called
(with a value `v`) only if `P v` holds; and 2- the person who executes
`perform (Suspend ...)` can assume that if(when) this subexpression returns a
value `v`, then `P v` holds. Furthermore, this person must prove that they
have provided a valid `register` function: which means that `register` must
not perform any effect (⊥ protocol) and must observe condition 1- above.
I see that you attempt to explain this in the paragraph
"The protocol for Suspend is entirely new [...]"
but I think that the text is too disconnected from the formal protocol.
More help should be given to the reader to decipher the formal part.
- [x] page 9, `promise_waiting γ`, in principle it should be possible to use a
meta_token to associate `γ` with `p`. You could then write `promise_waiting p`
and avoid ever mentioning `γ` to the reader/user.
- [ ] page 9, "The promise_waiting γ resource exists as two halves". It might be
worth clarifying what this means, i.e., what ghost updates or implications
you have. I imagine this:

  Initialisation
  (in the beginning we have promise_waiting p * promise_waiting p)

  Update
  promise_waiting p * promise_waiting p |=> promise_done p

  Contradiction
  promise_waiting p * promise_done p -∗ False
- [x] page 9, in `let '(p, γ, ε) := args`, what is ε? It seems unused.
- [x] page 9, in the definition of `ready`, `EWP` is used without a protocol.
Is it Coop?
- [ ] page 9, "PromiseInv still tracks"... "The Ready predicate now just
expresses..." You are still performing a comparison with Paulo's
code and proof, which you have not shown. Better explain your proof
first, and later discuss how it differs from Paulo's proof.
- [x] page 9, "As long as the promise is not fulfilled, it holds" : what holds?
Oh, "it holds and an instance of CQS" should be "it holds an instance of CQS".
Also, "an instance of CQS" sounds heavy. We should find a word for it, e.g.,
"a channel", or "a signaling channel".
- [ ] page 9, "so that wakers can be registered along with a resume_all_permit".
Not sure what "along ..." means. It sounds as if a resume_all_permit is
being registered at the same time as a waker. I don't think this is what
you mean. Also, `resume_all_permit` should be `signal_all_permit`.
- [x] page 9, "The Ready predicate now just expresses that f is safe". There
is no `f` in the definition of `ready`, it is `k`.
- [ ] page 9, "Now Ready is neither recursive nor mutually recursive with isPromise
anymore". This is interesting, and (in the paragraph where you compare with
Paulo's version) would deserve a little more explanation. You might wish to
recall why Ready and promiseInv were mutually recursive in Paulo's version.
You might also wish to indicate whether this tricky recursion has moved
elsewhere (somehow, it would be too good to be true, if it had disappeared
entirely). E.g., I see that now isRegister and Coop are mutually recursive,
and depend on promiseInv.
- [ ] page 9, it is not clear to me whether the abstraction boundaries (I mean, the
module boundaries) are respected in your logical-level definitions. Could you
clarify which definition is part of which module (in particular, Promise
versus Scheduler), and clarify which entities are visible to the outside world
(e.g., `isPromise` should be visible as an abstract predicate outside of the
`Promise` module, but `promiseInv` should ideally be entirely invisible
outside of this module)?
- [ ] Specifically, I am a bit worried to see that `isRegister` mentions
`promiseInv`. It seems to me that the Scheduler module should be entirely
unaware of the concept of "promise". Therefore `isRegister` should not mention
`promiseInv`.
Maybe `promiseInv` can be packaged inside an Iris invariant
so it becomes persistent and can remain hidden inside the
Promise module?
- [ ] page 9, "However, the specification only talks about [...]" I suggest first
explaining the specification of `run` (help the reader decipher it; explain
that it guarantees safety, no unhandled effects), and only later discuss its
limitations (it does not guarantee fairness or deadlock freedom).
- [x] page 10, "fairess"
- [ ] page 10, "hard to do in Iris" : probably true, but it may be worth trying to
explain (at the end of the thesis) why reasoning about fairness or liveness is
difficult (difficult in general and/or difficult specifically in Iris).
- [ ] page 10, "The proof proceeds as follows". This proof cannot be understood.
I think you will want to either remove the proof entirely or explain it in
greater detail: ideally, show the code of `run` again, annotated with the
iris assertion that holds at each point in the code, and comments explaining
why this assertion holds.
- [ ] page 10, "For this specification, the PromiseInv argument is needed to
interact with promises and the ewp proves that the new fiber is safe to
execute and obeys the Coop protocol. In return, the caller gets a promise that
will eventually hold a value satisfying the predicate Φ." This text is unclear
and needs to be improved (I think). At first I did not even understand that
this text is meant to be an explanation of the specification that follows.
- [ ] page 10, "The proof proceeds as follows", same criticism. It is impossible
to follow this proof without seeing the code.
- [ ] page 10, "we spin out its specification" : what does this mean?
- [x] page 10, "different to" -> "different from"
         "stil" -> "still"
- [x] page 10, what is `make_register`? There is no function by this name in the
code shown earlier, I think. I guess that `make_register` serves to isolate a
subexpression of `await` (beginning with `fun waker -> ...`). You should
explicitly isolate this function in the OCaml code, I think; it would probably
help understand `await`, which becomes fairly easy to understand if one
assumes that `make_register` does "the right thing". 

