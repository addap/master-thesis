(* TEST *)

open Printf
open Effect
open Effect.Deep

(* Declares a new constructor for the effect type 
 * E : int -> bool Effect.t *)
type _ eff += E : int -> bool eff

(* Evaluating a perform expression with a value of type 'a Effect.t 
 * transfers control to the enclosing handler and (possibly)
 * terminates in a value of type 'a. *)
let e () = 
  let (b : bool) = perform (E 1) in
  b

(* Evaluates the expression e () and if the effect E is performed,
 * control is transferred to the second branch.
 * The match acts as a deep handler, i.e. even if during the 
 * evaluation of e the effect E is performed multiple times, the
 * second branch is evaluated every time.
 * When e is reduced to a value, the non-effect branches are 
 * used for pattern matching as usual. *)
let _ = match e () with
  | v -> printf "result: %B\n%!" v
  (* This handler just checks if the passed value is 1.
   * Applying k to a value adds the continuation to the stack,
   * so control is transferred back to where the perform expression 
   * was evaluated. *)
  | effect (E v), k -> continue k (v = 1)