(* baexp.ml *)
open Abstract_Syntax   
(* Backward abstract interpretation of arithmetic operations *)
let rec b_aexp' a r p =
  match a with
  | (NAT i) -> if (Avalues.b_NAT i p) then r else (Aenv.bot ())
  | (VAR v) -> 
      (Aenv.set r v (Avalues.meet (Avalues.meet (Aenv.get r v) p) (Avalues.f_RANDOM ())))
  | RANDOM -> if (Avalues.b_RANDOM p) then r else (Aenv.bot ())
  | (UMINUS a1) -> (b_aexp' a1 r (Avalues.b_UMINUS (Aaexp.a_aexp a1 r) p))
  | (UPLUS a1)  -> (b_aexp' a1 r (Avalues.b_UPLUS (Aaexp.a_aexp a1 r) p))
  | (PLUS (a1, a2)) ->
    let (p1,p2) = (Avalues.b_PLUS (Aaexp.a_aexp a1 r) (Aaexp.a_aexp a2 r) p)
      in (Aenv.meet (b_aexp' a1 r p1) (b_aexp' a2 r p2))
  | (MINUS (a1, a2))  ->
    let (p1,p2) = (Avalues.b_MINUS (Aaexp.a_aexp a1 r) (Aaexp.a_aexp a2 r) p)
      in (Aenv.meet (b_aexp' a1 r p1) (b_aexp' a2 r p2))
  | (TIMES (a1, a2))  ->
    let (p1,p2) = (Avalues.b_TIMES (Aaexp.a_aexp a1 r) (Aaexp.a_aexp a2 r) p)
      in (Aenv.meet (b_aexp' a1 r p1) (b_aexp' a2 r p2))
  | (DIV (a1, a2))  ->
    let (p1,p2) = (Avalues.b_DIV (Aaexp.a_aexp a1 r) (Aaexp.a_aexp a2 r) p)
      in (Aenv.meet (b_aexp' a1 r p1) (b_aexp' a2 r p2))
  | (MOD (a1, a2))  ->
    let (p1,p2) = (Avalues.b_MOD (Aaexp.a_aexp a1 r) (Aaexp.a_aexp a2 r) p)
      in (Aenv.meet (b_aexp' a1 r p1) (b_aexp' a2 r p2))
let b_aexp a r p =
 if (Aenv.is_bot r) & (Avalues.isbotempty ()) then (Aenv.bot ()) else b_aexp' a r p
