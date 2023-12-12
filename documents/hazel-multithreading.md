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

To add multi-threading to hazel we want to use the same approach.
The question is, do effect handlers and multi-threading interact in any complicated way which would make it difficult to add multi-threaded reduction rules.  
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
`Eff` does not have a direct analogon because beta reduction is normally just a single step, but the continuation `k` built up by reducing an `Eff v k` is a reification of the evaluation context that is often used to define operational semantics.

In addition to this, reduction of an `Eff v k` can only happen under some evaluation context.
Reduction under an empty context is undefined, i.e. the program crashes.
Due to the way we define multi-threading, the forked-off expression does not have any relation to the original expression, for example in the form of an evaluation context.
So there is no possibility for effects to "cross a thread boundary" or to "handle the effects of another thread", effects are performed and handled only in the same thread.

Therefore, we see that effect handlers do not interact with multi-threading in any complicated way because they just enable a new form of reduction localized to a single thread.
For the implementation this means concretely that effect handlers only add rules to the thread reduction $\to_t$, while muti-threading adds a new output to the thread reduction $\to_t$ and defines the multi-threaded reduction $\to_{mt}$ on top.
Also when proving the safety of an expression via EWP we must ensure that a forked-off expression handles all its effects, so that the program does not crash.

## Implementing Multi-Threading

This entails adding `fork` and `cmpxchg` primitive operations to the language, proving their reasoning rules, and updating the definition of EWP to include forked-off threads.
Because of this the soundness proof of EWP will need to be updated, but since effects and multi-threading are orthogonal the necessary changes are simple.
This in more detail
