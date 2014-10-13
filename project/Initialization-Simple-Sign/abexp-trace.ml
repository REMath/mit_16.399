(* abexp.ml *)
open Abstract_Syntax
open Avalues
open Aenv
open Aaexp
(* abstract interpretation of boolean operations *)
let rec a_bexp b r =
 let r' =  match b with
  | TRUE           -> r
  | FALSE          -> (Aenv.bot ())
  | (EQ (a1, a2))  -> f_EQ (a_aexp a1) (a_aexp a2) r
  | (LT (a1, a2))  -> f_LT (a_aexp a1) (a_aexp a2) r
  | (AND (b1, b2)) -> Aenv.meet (a_bexp b1 r) (a_bexp b2 r)
  | (OR (b1, b2))  -> Aenv.join (a_bexp b1 r) (a_bexp b2 r)
 in
  print_string "a_bexp[";
  Pretty_Print.print_Bexp b; print_string "]("; Aenv.print r;print_string ") = ";
  Aenv.print r';print_newline ();
  r'
