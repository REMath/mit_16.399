(* aexp.ml *)
open Abstract_Syntax   
open Values
open Env
(* evaluation of arithmetic operations *)
let rec eval_aexp e r = match e with
| Abstract_Syntax.NAT i -> machine_int_of_string i
| VAR v        -> get r v
| RANDOM       -> machine_unary_random ()
| UPLUS a      -> machine_unary_plus (eval_aexp a r)
| UMINUS a     -> machine_unary_minus (eval_aexp a r)
| PLUS (a, b)  -> machine_binary_plus (eval_aexp a r) (eval_aexp b r)
| MINUS (a, b) -> machine_binary_minus (eval_aexp a r) (eval_aexp b r)
| TIMES (a, b) -> machine_binary_times (eval_aexp a r) (eval_aexp b r)
| DIV (a, b)   -> machine_binary_div (eval_aexp a r) (eval_aexp b r)
| MOD (a, b)   -> machine_binary_mod (eval_aexp a r) (eval_aexp b r)
