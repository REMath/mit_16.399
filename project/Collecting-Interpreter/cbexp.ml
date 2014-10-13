(* cbexp.ml *)
open Abstract_Syntax
open Cvalues
open Cenv
open Caexp
(* evaluation of boolean operations *)
let rec c_bexp b r =
  match b with
  | TRUE           -> r
  | FALSE          -> (Cenv.bot ())
  | (EQ (a1, a2))  -> f_EQ (c_aexp a1) (c_aexp a2) r
  | (LT (a1, a2))  -> f_LT (c_aexp a1) (c_aexp a2) r
  | (AND (b1, b2)) -> Cenv.meet (c_bexp b1 r) (c_bexp b2 r)
  | (OR (b1, b2))  -> Cenv.join (c_bexp b1 r) (c_bexp b2 r)
