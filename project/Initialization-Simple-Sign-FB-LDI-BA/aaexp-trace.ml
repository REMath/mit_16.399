(* aaexp.ml *)
open Abstract_Syntax   
(* Abstract interpretation of arithmetic operations *)
let rec a_aexp a r = 
 let v = match a with
  | (Abstract_Syntax.NAT i) -> (Avalues.f_NAT i)
  | (VAR v)      -> (Aenv.get r v)
  | RANDOM       -> Avalues.f_RANDOM ()
  | (UPLUS a1)   -> (Avalues.f_UPLUS (a_aexp a1 r))
  | (UMINUS a1)  -> (Avalues.f_UMINUS (a_aexp a1 r))
  | (PLUS (a1, a2))  -> (Avalues.f_PLUS (a_aexp a1 r) (a_aexp a2 r))
  | (MINUS (a1, a2)) -> (Avalues.f_MINUS (a_aexp a1 r) (a_aexp a2 r))
  | (TIMES (a1, a2)) -> (Avalues.f_TIMES (a_aexp a1 r) (a_aexp a2 r))
  | (DIV (a1, a2))   -> (Avalues.f_DIV (a_aexp a1 r) (a_aexp a2 r))
  | (MOD (a1, a2))   -> (Avalues.f_MOD (a_aexp a1 r) (a_aexp a2 r))
 in
   print_string "a_aexp[";
   Pretty_Print.print_Aexp a; print_string "]("; Aenv.print r;print_string ") = ";
   Avalues.print v;print_newline ();
   v