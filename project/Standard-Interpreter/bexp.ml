(* bexp.ml *)
open Abstract_Syntax
open Values
open Env
open Aexp
(* evaluation of boolean operations *)
let rec eval_bexp b r =
  match b with
  | TRUE           -> (BOOLEAN true)
  | FALSE          -> (BOOLEAN false)
  | (EQ (a1, a2))  -> machine_eq (eval_aexp a1 r) (eval_aexp a2 r)
  | (LT (a1, a2))  -> machine_lt (eval_aexp a1 r) (eval_aexp a2 r)
  | (AND (b1, b2)) -> machine_and (eval_bexp b1 r) (eval_bexp b2 r)
  | (OR (b1, b2))  -> machine_or (eval_bexp b1 r) (eval_bexp b2 r)
