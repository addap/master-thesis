TODO pretty old and cancellation does not work like this anyways so inherently flawed.

# Verification of Schedulers with Cancellation

Eio's model of cancellation uses a cancellation context `cancel_ctx` associated with a tree of fibers.
Each cancellation context contains a reference `cf`, which if set to `true` means all fibers in the tree are cancelled.

We simplify it to associate each cancellation context with exactly one fiber.
We also add an I/O log `state` to the cancellation context (at the moment only a nat that is incremented during an IO operation), and we want to verify the statement that after cancellation this log does not change anymore.

## Formalization of Cancellation

Cancellation has two main functions with two effects:

```ocaml
type _ Effect.t += Fail : () -> void Effect.t (* does not return *)
type _ Effect.t += GetContext : () -> cancel_ctx Effect.t

fiber_cancel ((cf, state): cancel_ctx) : nat =
    cf <- true;
    !state

io : () =
    let (cf, state) = get_ctx ();
    if !cf
    then fail ()
    else
        state <- !state + 1
        () (* do the actual operation *)
```

Forking a new fiber returns a promise along with the child's cancellation context, which can be used afterwards to call `fiber_cancel` on the child.
`io` uses the `GetContext` effect to get access to a fiber's own context and check the cancellation flag.

This design is problematic with the proof structure of the scheduler case study in Hazel because there are two (or more) fibers with access to the cancellation context, the parent fiber and the child via an effect.

For the points-to-assertions of a cancellation context, I used the same approach as is already used for promises in the scheduler case study.
`cancelInv` hold an authoritative map for the ghost names and locations.
Then for each `cf, state, δ`, it also holds the points-to-assertions `cancelSt`.
`io_log_{active,cancelled}` is a oneshot RA where the cancelled variant is duplicable and unchangeable so that we know that `i` cannot change anymore.
`isCancel` is the fragment used to temporarily own a `cancelSt`.

```coq
Definition cancelInv : iProp Σ := (
∃ M, (own cancel_name (● M)) ∗
    [∗ map] args ↦ tt ∈ M, let '(cf, state, δ) := args in
    (* below is called cancelSt *)
    ∃ (i: nat), state ↦ #i ∗ (
        (cf ↦ #false ∗ io_log_active δ i)
    ∨
        (cf ↦ #true ∗ io_log_cancelled δ i)
    )
)%I.

Definition isCancel (cf state : loc) : iProp Σ := (
    ∃ δ, own cancel_name (◯ {[(cf, state, δ) := tt]}).
).
```

## Problem

The proof for the effect handler uses `cancelInv` but that means that only the effect handler can own the `cancelSt` to change the `cancel_ctx` of each fiber.
An effect like `GetContext` should temporarily lend out a `cancelSt` but I was not able to formulate the effect in Hazel.
`fiber_cancel` therefore also does not have the right to change the `cancel_ctx`.

## Possible Solutions

### 1. Move code that accesses cancel_ctx into the effect handler

I was able to verify a variation where instead of requesting the permission to use the points-to-assertion from the effect handler, one gives the effect handler a callback.
This results in similar code as above but is a bit awkward.
Also, we cannot perform the Fail effect inside the callback and must instead return a bool to perform Fail afterwards.

```ocaml
type _ Effect.t += Fail : () -> void Effect.t (* does not return *)
type _ Effect.t += WithContext : (cancel_ctx -> a) -> a Effect.t
type _ Effect.t += WithOtherContext : cancel_ctx -> (cancel_ctx -> a) -> a Effect.t

fiber_cancel (cc: cancel_ctx) : nat =
    let res = with_other_context cc (fn (cf, state) ->
        cf <- true;
        !state) in
    res

io : () =
    let cancelled = get_ctx (fn (cf, state) ->
        if !cf
        then true
        else
            state <- !state + 1
            false)
    if cancelled
    then fail ()
    else () (* do the actual operation *)
```

Using this approach it's possible to prove the following lemmas about `io` and `fiber_cancel`.
Unfortunately `io` does not have much of a specification.
But for `fiber_cancel` we know that the natural number that is returned cannot be changed afterwards.

The WithContext effects take a callback that gets access to a `cancelSt`.

```coq
Definition WITH_OTHER_CONTEXT : iEff Σ :=
  >> (cf state : loc) P f >> !(WithOtherContext' ((#cf, #state), f)%V)
  {{isCancel cf state ∗ ∀ δ, cancelSt cf state δ -∗
    EWP f (#cf, #state)%V <| ⊥ |> {{v, P v ∗ cancelSt cf state δ}} }};
  << v << ?(v) {{P v}} @ OS.

Definition WITH_CONTEXT : iEff Σ :=
  >> P f >> !(WithContext' f) {{∀ cf state δ, cancelSt cf state δ -∗
    EWP f (#cf, #state)%V <| ⊥ |> {{v, P v ∗ cancelSt cf state δ}} }};
  << v << ?(v) {{P v}} @ OS.

Lemma ewp_io :
  ⊢ EWP io #() <| Coop |> {{ _, True }}.

Lemma ewp_fiber_cancel (cf state : loc) :
  isCancel cf state -∗
  EWP (fiber_cancel (#cf, #state)%V) <| Coop |> {{ v, ∃ (δ: gname) (i: nat), ⌜ v = #i ⌝ ∗ io_log_cancelled δ i }}.

Lemma ewp_async (e : val) Φ :
  □ Φ RNone' ∗ EWP e #() <| Coop |> {{ v, □ Φ (RSome' v) }} ⊢
    EWP (async e) <| Coop |> {{ y,
      ∃ (p : loc) (cf state : loc), ⌜ y = (#p, (#cf, #state))%V ⌝ ∗ isPromise p Φ ∗ isCancel cf state }}.
```

### 2. Pass the cancelInv around.

It might be possible to pass `cancelInv` from the effect handler to each running fiber.
E.g. when a new fiber is scheduled to run, it runs with `cancelInv` and can therefore call the original `fiber_context`.
Performing an effect passes `cancelInv` back to the handler (which might schedule another fiber before returning to you) and you get `cancelInv` back in the postcondition of the effect.

I did not yet to look into how to restructure the proof for this.

### 3. Change the formulation of protocols

This is what I was originally thinking about, but 2. might be the easier solution.
Right now, protocols are a two-step communication where a client first gives up resource `P` and will get back resource `Q` from the handler.

```
! x (v) {P}. ? y (w) {Q} allows do v' {Φ}
  ⊣⊢ ∃ x, ⌜ v' = v ⌝ ∗ P ∗ ∀ y, Q -∗ Φ(w)
```

For an effect like `GetContext : () -> cancel_ctx Effect.t` if might be useful to have a three-step communication where the client temporarily gains access to resource `R` but must give it up before performing the next effect.

I tried something like the following, where a client must give up an `effectTok` to perform an effect.
In return it receives `R` along with a function to give up `R` and get back the `effectTok`.

```
! x (v) {P}. ? y (w) {Q} ⭯ {R} allows do v' {Φ}
  ⊣?⊢ ∃ x, ⌜ v' = v ⌝ ∗ effectTok ∗ P ∗ ∀ y, (Q ∗ R ∗ (R -∗ effectTok)) -∗ Φ(w)
```

But this does not work for the handler since often `R` is used in the proof of `Φ`.
So I'm not sure if the "client temporarily gains access to `R`" can be encoded in protocols.
Explicitly threadying `cancelInv` through pre- and postconditions should have the same effect.
