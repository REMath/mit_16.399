(* bcom.ml *)
open Linear_Syntax
open Aenv
open Abexp
open Fixpoint
(* backward abstract semantics of commands *)
exception Error of string
let rec bcom c r l =
  match c with
  | (LSKIP (l', l'')) -> 
      if (l = l') then r
      else if (l = l'') then r
      else (raise (Error "SKIP incoherence"))
  | (LASSIGN (l',x,a,l'')) ->
      if (l = l') then b_ASSIGN x a r
      else if (l = l'') then r
      else (raise (Error "ASSIGN incoherence"))
  | (LSEQ (l', s, l'')) -> 
      (bcomseq s r l)
  | (LIF (l', b, nb, t, f, l'')) ->
      (if (l = l') then 
         (join (a_bexp b (bcom t r (at t))) (a_bexp nb (bcom f r (at f))))
      else if (incom l t) then
         (bcom t r l)
      else if (incom l f) then
         (bcom f r l)
      else if (l = l'') then 
         r
      else (raise (Error "IF incoherence")))
  | (LWHILE (l', b, nb, c', l'')) ->
    let f x = (join (a_bexp b (bcom c' x (at c'))) (a_bexp nb r))
    in let i = lfp (bot ()) leq widen narrow f in
      (if (l = l') then i
       else if (incom l c') then (bcom c' i l)
       else if (l = l'') then r
       else (raise (Error "WHILE incoherence")))
and bcomseq s r l = match s with
  | []   -> raise (Error "empty SEQ incoherence")
  | [c]  -> if (incom l c) then (bcom c r l) 
            else (raise (Error "SEQ incoherence"))
  | h::t -> if (incom l h) then (bcom h (bcomseq t r (at (List.hd t))) l) 
            else (bcomseq t r l)
