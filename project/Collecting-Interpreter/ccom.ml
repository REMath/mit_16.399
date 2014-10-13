(* ccom.ml *)
open Abstract_Syntax
open Labels
open Cenv
open Caexp
open Cbexp
open Fixpoint
(* collecting semantics of commands *)

exception Error of string
let rec ccom c r l =
  match c with
  | (SKIP (l', l'')) -> 
      if (l = l') then r
      else if (l = l'') then r
      else (raise (Error "SKIP incoherence"))
  | (ASSIGN (l',x,a,l'')) ->
      if (l = l') then r
      else if (l = l'') then
			  f_ASSIGN x (c_aexp a) r
      else (raise (Error "ASSIGN incoherence"))
  | (SEQ (l', s, l'')) -> 
      (ccomseq s r l)
  | (IF (l', b, nb, t, f, l'')) ->
      (if (l = l') then r
      else if (incom l t) then
         (ccom t (c_bexp b r) l)
      else if (incom l f) then
         (ccom f (c_bexp nb r) l)
      else if (l = l'') then
         (join (ccom t (c_bexp b r) (after t))
				       (ccom f (c_bexp nb r) (after f)))
      else (raise (Error "IF incoherence")))
  | (WHILE (l', b, nb, c', l'')) ->
    let f x = join r (ccom c' (c_bexp b x) (after c'))
    in let i = lfp (bot ()) leq f in
      (if (l = l') then i
       else if (incom l c') then (ccom c' (c_bexp b i) l)
       else if (l = l'') then (c_bexp nb i)
       else (raise (Error "WHILE incoherence")))
and ccomseq s r l = match s with
  | []   -> raise (Error "empty SEQ incoherence")
  | [c]  -> if (incom l c) then (ccom c r l) 
            else (raise (Error "SEQ incoherence"))
  | h::t -> if (incom l h) then (ccom h r l) 
            else (ccomseq t (ccom h r (after h)) l)

