(* caexp.ml *)
open Abstract_Syntax   
(* evaluation of arithmetic operations *)
let rec c_aexp a r = match a with
  | (Abstract_Syntax.NAT i) -> (Cvalues.f_NAT i)
  | (VAR v)      -> (Cenv.get r v)
  | RANDOM       -> Cvalues.f_RANDOM ()
  | (UPLUS a1)   -> (Cvalues.f_UPLUS (c_aexp a1 r))
  | (UMINUS a1)  -> (Cvalues.f_UMINUS (c_aexp a1 r))
  | (PLUS (a1, a2))  -> (Cvalues.f_PLUS (c_aexp a1 r) (c_aexp a2 r))
  | (MINUS (a1, a2)) -> (Cvalues.f_MINUS (c_aexp a1 r) (c_aexp a2 r))
  | (TIMES (a1, a2)) -> (Cvalues.f_TIMES (c_aexp a1 r) (c_aexp a2 r))
  | (DIV (a1, a2))   -> (Cvalues.f_DIV (c_aexp a1 r) (c_aexp a2 r))
  | (MOD (a1, a2))   -> (Cvalues.f_MOD (c_aexp a1 r) (c_aexp a2 r))
