(* acom.ml *)
open Abstract_Syntax
open Labels
open Aenv
open Aaexp
open Abexp
open Fixpoint
(* forward abstract semantics of commands *)

exception Error of string
let rec acom c r l =
 match c with
 | (SKIP (l', l'')) -> 
     if (l = l') then r
     else if (l = l'') then r
     else (raise (Error "SKIP incoherence"))
 | (ASSIGN (l',x,a,l'')) ->
     if (l = l') then r
     else if (l = l'') then
               f_ASSIGN x (a_aexp a) r
     else (raise (Error "ASSIGN incoherence"))
 | (SEQ (l', s, l'')) -> 
     (acomseq s r l)
 | (IF (l', b, nb, t, f, l'')) ->
     (if (l = l') then r
     else if (incom l t) then
        (acom t (a_bexp b r) l)
     else if (incom l f) then
        (acom f (a_bexp nb r) l)
     else if (l = l'') then
        (join (acom t (a_bexp b r) (after t)) 
	      (acom f (a_bexp nb r) (after f)))
     else (raise (Error "IF incoherence")))
 | (WHILE (l', b, nb, c', l'')) ->
   let f x = join r (acom c' (a_bexp b x) (after c'))
   in let i = lfp (bot ()) leq f in
     (if (l = l') then i
      else if (incom l c') then (acom c' (a_bexp b i) l)
      else if (l = l'') then (a_bexp nb i)
      else (raise (Error "WHILE incoherence")))
and acomseq s r l = match s with
 | []   -> raise (Error "empty SEQ incoherence")
 | [c]  -> if (incom l c) then (acom c r l) 
           else (raise (Error "SEQ incoherence"))
 | h::t -> if (incom l h) then (acom h r l) 
           else (acomseq t (acom h r (after h)) l)

