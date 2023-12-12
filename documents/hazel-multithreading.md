# Adding Multi-Threading to Hazel

## How Effect Handlers and Multi-Threading Interact

Hazel is an untyped ML-like language with recursive functions, higher-order references and effect handlers.
Apart from effect handlers, it is well-known how to add multi-threading to such a language.
Heaplang does it by defining a reduction relation over a set of threads where each step picks a random thread and reduces it according to the thread reduction relation.
A step in the thread reduction relation can fork off a new thread which gets added to the set.

$$
  (e, \sigma) \to_t (e', \sigma', ef)\\
  (efs_1 + [e] + efs_2, \sigma) \to_{mt} (efs_1 + [e'] + efs_2 + [ef], \sigma')
$$

To add multi-threading to hazel we use the same approach.
The question is, do effect handlers and multi-threading interact in any complicated way which would make it difficult to integrate these reduction rules.  
It turns out that they do not.
In the following we explain why effect handlers as defined by Hazel can be seen as just another way of defining and calling functions localized to a single thread.

Effect handlers in Hazel are implemented by three expressions, `Do` `Eff` and `TryWith`, and their rules in the reduction relation.

- `TryWith e1 e2 e3` defines an effect handler. It reduces `e1` to a value `v` and then reduces `e3 v`.
- An `Eff v k` expression is an effect and reduces by building up a continuation using its context, i.e. there are rules like $Fst\; (Eff\; v\; k) \to_h Eff\; v\; (FstCtx :: k)$ for many expressions.
  These `Eff` reduction rules are applied until the outer context is a handler, `TryWith (Eff v k) e2 e3`, at which point reduction switches to the handler's effect branch `e2`.
- Finally, a `Do v` expression just wraps its given value in an `Eff v []` expression, thereby switching to the above mentioned `Eff` reduction rules.

The combination of these rules results in the expected behavior of effect handlers.
Phrased like this, we can view function calls inherent in a lambda calculus as a rough analogy to the behavior of effect handlers:
`TryWith` defines an anonymous function and `Do` calls the function.
`Eff` does not have a direct analogon because beta reduction is normally just a single step, but the continuation `k` built up by reducing an `Eff v k` is the reification of the evaluation context that is often used to define operational semantics.

In addition to this, reduction of an `Eff v k` can only happen under some evaluation context.
Reduction under an empty context is undefined, i.e. the program crashes.
Due to the way we define multi-threading, the forked-off expression does not have any relation to the original expression, for example in the form of an evaluation context.
So there is no possibility for effects to "cross a thread boundary" or to "handle the effects of another thread", effects are performed and handled only in the same thread.

Therefore, we see that effect handlers do not interact with multi-threading in any complicated way because they just enable a new form of function call-like reduction localized to a single thread.
The only rule we need to impose is that when proving the safety of an expression via EWP we must ensure that a forked-off expression handles all its effects, so that the program does not crash.
This is explained in the following section.

## Implementing Multi-Threading

To add multi-threading to Hazel we add the `Fork` and `CmpXchg` expressions to the language, prove their reasoning rules, and update the definition of EWP to include forked-off threads.

1. Adding the expressions also entails adding new evaluation context frames for `CmpXchg`. Since `Fork e` forks off the whole expression without evaluating it first we do not add an evaluation context frame for it.

```coq
Inductive expr :=
  ...
  (* Concurrency *)
  | CmpXchg (e1 : expr) (e2 : expr) (e3 : expr)
  | Fork (e : expr)
with frame :=
  ...
  | CmpXchgLCtx (v1 : val) (v2 : val)
  | CmpXchgMCtx (e1 : expr) (v2 : val)
  | CmpXchgRCtx (e1 : expr) (e2 : expr).
```

2. We add reduction rules which are the same as in Heaplang.

```coq
Definition head_step :=
  ...
  | CmpXchgS l v1 v2 vl σ b :
     σ.(heap) !! l = Some vl →
     vals_compare_safe vl v1 →
     b = bool_decide (vl = v1) →
      head_step (CmpXchg (Val $ LitV $ LitLoc l) (Val v1) (Val v2)) σ
               (Val $ PairV vl (LitV $ LitBool b)) (if b then heap_upd <[l:=v2]> σ else σ)
               []
  | ForkS e σ :
     head_step (Fork e) σ (Val $ LitV LitUnit) σ [e]
```

3. We amend the definition of EWP to include forked-off threads analogous to Heaplang, by requiring an EWP for them.
   The only new requirement is that forked-off threads must obey the empty protocol.

```coq
Definition ewp_pre `{!irisGS eff_lang Σ} :
  := λ ewp E e₁ Ψ₁ Φ,
  match to_val e₁ with
  | Some v =>
      |={E}=> Φ v
  | None =>
      match to_eff e₁ with
      | Some (v, k) =>
          iEff_car (upcl OS Ψ₁) v (λ w, ▷ ewp E (fill k (Val w)) Ψ₁ Φ)
      | None =>
          ∀ σ₁ ns κ κs nt,
            state_interp σ₁ ns (κ ++ κs) nt ={E,∅}=∗
              ⌜ reducible e₁ σ₁ ⌝ ∗
              ∀ e₂ σ₂ efs, ⌜ prim_step' e₁ σ₁ κ e₂ σ₂ efs ⌝
                ={∅}▷=∗^(S $ num_laters_per_step ns) |={∅,E}=>
                state_interp σ₂ (S ns) κs (length efs + nt) ∗ ewp E e₂ Ψ₁ Φ ∗
                [∗ list] i ↦ ef ∈ efs, ewp ⊤ ef ⊥ fork_post (* <-- NEW: forked-off threads must obey the empty protocol. *)
      end
  end%I.
```

4. The reasoning rule `ewp_fork` follows directly from the definition of EWP and the reduction rule for `Fork`.

```coq
Lemma ewp_fork E e Ψ1 Φ :
  ▷ EWP e @ ⊤ <| ⊥ |> {{ fork_post }} -∗ ▷ Φ (LitV LitUnit) -∗
    EWP Fork e @ E <| Ψ1 |> {{ Φ }}.
```

5. The reasoning rules `ewp_cmpxchg_suc` & `ewp_cmpxchg_fail` also follow from the definition of EWP and the reduction rules for `CmpXchg`.

```coq
Lemma ewp_cmpxchg_fail E Ψ1 Φ l v' v1 v2 :
  v' ≠ v1 → vals_compare_safe v' v1 →
  ▷ l ↦ v' -∗
    ▷ (l ↦ v' ={E}=∗ Φ (PairV v' (LitV $ LitBool false))) -∗
      EWP (CmpXchg (Val $ LitV $ LitLoc l) (Val v1) (Val v2)) @ E <| Ψ1 |> {{ Φ }}.
```

6. The adequacy lemma of EWP leverages adequacy of the WP predicate generated by Iris.
   They prove that EWP implies WP, which mostly follows by the reduction rules.
   We update the proof of the adequacy lemma to include that an EWP of all forked-off threads implies a WP of all forked-off threads.
   This works by simple induction over the list of forked-off threads.

```coq
Lemma ewp_imp_wp `{!heapGS Σ} E e Φ :
  EWP e @ E <| ⊥ |> {{ Φ }} -∗ WP e @ NotStuck; E {{ Φ }}.
```

So we have added multi-threading primitives to the language, proved their basic reasoning rules and proved adequacy of the resulting EWP.
We can now write multi-threaded programs in Hazel and prove them safe.

TODO maybe note that we had to rearrange some context variables from irisGS to heapGS, which made the definition of num_laters_per_step transparent and simplified some proofs.
