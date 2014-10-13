(* acom.ml *)
open Linear_Syntax
open Aenv
open Abexp
open Fixpoint
(* collecting semantics of commands *)

exception Error of string
let rec acom c r l =
  match c with
  | (LSKIP (l', l'')) -> 
      if (l = l') then r
      else if (l = l'') then r
      else (raise (Error "SKIP incoherence"))
  | (LASSIGN (l',x,a,l'')) ->
      if (l = l') then r
      else if (l = l'') then
                f_ASSIGN x a r
      else (raise (Error "ASSIGN incoherence"))
  | (LSEQ (l', s, l'')) -> 
      (acomseq s r l)
  | (LIF (l', b, nb, t, f, l'')) ->
      (if (l = l') then r
      else if (incom l t) then
         (acom t (a_bexp b r) l)
      else if (incom l f) then
         (acom f (a_bexp nb r) l)
      else if (l = l'') then
                 (let rt = (acom t (a_bexp b r)  (after t))
                  and rf = (acom f (a_bexp nb r) (after f))
                  in join rt rf)
      else (raise (Error "IF incoherence")))
  | (LWHILE (l', b, nb, c', l'')) ->
    let f x = join r (acom c' (a_bexp b x) (after c'))
    in let i = lfp (bot ()) leq widen narrow f in
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

