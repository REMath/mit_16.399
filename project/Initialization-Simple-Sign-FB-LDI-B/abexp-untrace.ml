(* abexp.ml *)
open Abstract_Syntax
open Fixpoint
(* abstract interpretation of boolean operations with iterative *)
(* reduction                                                    *)
let rec a_bexp' b r =
 match b with
 | TRUE         -> r
 | FALSE        -> (Aenv.bot ())
 | EQ (a1, a2)  ->
   let (p1,p2) = (Avalues.a_EQ (Aaexp.a_aexp a1 r) (Aaexp.a_aexp a2 r))
    in (Aenv.meet (Baexp.b_aexp a1 r p1) (Baexp.b_aexp a2 r p2))
 | LT (a1, a2)  ->
   let (p1,p2) = (Avalues.a_LT (Aaexp.a_aexp a1 r) (Aaexp.a_aexp a2 r))
    in (Aenv.meet (Baexp.b_aexp a1 r p1) (Baexp.b_aexp a2 r p2))
 | AND (b1, b2) -> (Aenv.meet (a_bexp' b1 r) (a_bexp' b2 r))
 | OR (b1, b2)  -> (Aenv.join (a_bexp' b1 r) (a_bexp' b2 r))
let a_bexp b r =
  if (Aenv.is_bot r) & (Avalues.isbotempty ()) then (Aenv.bot ()) 
  else gfp r Aenv.leq (a_bexp' b)