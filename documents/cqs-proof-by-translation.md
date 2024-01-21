# Transporting CQS proofs

This is an idea how we could reuse the existing specifications for the CQS operations in our case study **without** manually translating the programs and proofs to Hazel.

## Why

Translating the program wouldn't be so bad, but the proofs are still about 5000 lines of Coq code and there might be some ways where I cannot use the exact same tactics, which can quickly become very time consuming.
I would need to fix the `pure_step` tactic before I even begin, as well as integrating Hazel invariants with the `iInv` tactic.

Also, apart from some engineering challenges it's not very interesting.
The other approach below seems like it can save me a lot of time and actually be an interesting result for future usage of Hazel.

## How

I believe that every program in a restricted syntax of heaplang (i.e. without prophecy) which is heap-closed (i.e. does only access heap locations that it allocated itself) is a valid Hazel program.
Also, every WP proven for such a program `e` carries over to an EWP with the empty protocol for a translated program `[[e]]`.
I also believe that CQS falls into the restricted syntax: It does not use prophecy variables & while it handles locations passed from outside, it never reads from them, so it is heap-closed.
Therefore I think we can define a (partial) translation `[[ _ ]]` from heaplang to Hazel, and prove two lemmas about it:

0. We call expressions that use restricted syntax and are heap-closed "translatable".
   We need inversion rules like: subexpressions of a translatable expression are also translatable. Also, the translation of a subexpression is a subexpression of the translation of the original expression.
1. For every step done by a translatable expression there exists an equivalant step for the translated Hazel expression. If a new variable is allocated in heaplang, we also add it for the step in Hazel.
2. For a translatable expression we prove `WP e {{ v, Q v }} -> EWP [[e]] {{ w, ∃ v, [[v]] = w ∧ Q v }}`

## Problems

- One of the biggest problems for the translation is that in the definition of WP/EWP there is a universally quantified heap. For EWP it of course contains Hazel values, but if we give this heap to a heaplang expression we need to translate the values to heaplang values. Since there are Hazel values that we cannot translate we restrict the heaplang expressions to those that do not access the heap except for values they allocate themselves.
- It is not clear how to syntactically define expressions that only access the heap for locations that they allocate themselves.
  - Also, when we do one step from e to e', then e' could access a location that e allocated. So we should track which locations were allocated over the course of evaluation and allow access to these.

```coq
∀ (e : heap_lang) (c : Hazel),
    translatable e -> [[e]] = Some c ->
    ∀ σ σ' (e' : heap_lang), (e, σ) ~> (e', σ') ->
        ∃ (c' : eff_lang), translatable e' ∧ [[e']] = Some c' ∧ (c, σ) ~> (c', σ')
```
