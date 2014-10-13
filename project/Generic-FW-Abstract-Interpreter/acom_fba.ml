(* acom_bfa *)
open Abstract_Syntax
open Labels
open Aenv
open Aaexp
open Abexp
open Fixpoint
open Baexp
open Trace
(* Abstract interpretation of commands with forward/backward analysis of *)
(* assignment                                                            *)
exception Error of string
let tracelabenv l r r' = if trace_com () then
      (print_string "Acom["; print_label l; print_string ":]("; Aenv.print r; print_string ") = "; Aenv.print r'; print_newline (); r')
   else
	    r'
let rec acom c r l =
  match c with
  | (SKIP (l', l'')) -> 
	    if (l = l'') then (tracelabenv l'' r r)
      else (raise (Error "SKIP incoherence"))
  | (ASSIGN (l',x,a,l'')) ->
      if (l = l'') then
			(tracelabenv l'' r (f_ASSIGN x (a_aexp a) (b_aexp a r (Avalues.f_RANDOM ()))))
      else (raise (Error "ASSIGN incoherence"))
  | (SEQ (l', s, l'')) -> 
      (acomseq s r l)
  | (IF (l', b, nb, t, f, l'')) ->
      (if (l = l'') then
			   let att = (tracelabenv (at t) r (a_bexp b r)) in
				 let aftert = (acom t att (after t)) in
				 let atf = (tracelabenv (at f) r (a_bexp nb r)) in
				 let afterf = (acom f atf (after f)) in
				    (tracelabenv l r (join aftert afterf))
      else (raise (Error "IF incoherence")))
  | (WHILE (l', b, nb, c', l'')) ->
    let f x = join r (acom c' (tracelabenv (at c') r (a_bexp b x)) (after c'))
    in let i = lfp (bot ()) leq widen narrow f in
      (if (l = l'') then (tracelabenv l r (a_bexp nb i))
       else (raise (Error "WHILE incoherence")))
and acomseq s r l = match s with
  | []   -> raise (Error "empty SEQ incoherence")
  | [c]  -> if (incom l c) then (acom c r l) 
            else (raise (Error "SEQ incoherence"))
  | h::t -> if (incom l h) then (acom h r l) 
            else let hr = (acom h r (after h)) in (acomseq t hr l)

