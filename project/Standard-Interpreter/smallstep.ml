(* smallstep.ml *)
open Abstract_Syntax
open Labels
open Values
open Env
open Aexp
open Bexp
(* program states *)
type state = label * env
(* small-step operational semantics of commands *)

exception Error of string
let rec trans c (l, r) = match c with
| (SKIP (l', l'')) -> 
    if (l = l') then (l'', r)
    else (raise (Error "SKIP incoherence"))
| (ASSIGN (l',x,a,l'')) ->
    if (l = l') then 
      (let v = (eval_aexp a r) in
         (set r x v;
          (l'', r)))
    else (raise (Error "ASSIGN incoherence"))
| (SEQ (l', s, l'')) -> 
    (transseq s (l, r))
| (IF (l', b, nb, t, f, l'')) ->
    (if (l = l') then
       (match (eval_bexp b r) with
        | ERROR_BOOL e -> (raise (Error ("runtime error in \"if\" at " 
	                                 ^ (string_of_label l))))
        | BOOLEAN true -> ((at t), r)
        | BOOLEAN false -> match (eval_bexp nb r) with
            | ERROR_BOOL e -> 
              (raise (Error ("runtime error in \"if\" at " 
	                     ^ (string_of_label l))))
            | BOOLEAN true -> ((at f), r)
            | BOOLEAN false -> (raise (Error "IF test incoherence")))
     else if (l = (after t)) then (l'', r)
     else if (l = (after f)) then (l'', r)
     else if (incom l t) then
        (trans t (l, r))
     else if (incom l f) then
        (trans f (l, r))
     else (raise (Error "IF incoherence")))
| (WHILE (l', b, nb, c', l'')) ->
  (if (l = l') then
     (match (eval_bexp b r) with
      | ERROR_BOOL e -> (raise (Error 
       ("runtime error in \"while\" loop at " ^ (string_of_label l))))
      | BOOLEAN true -> ((at c'), r)
      | BOOLEAN false -> match (eval_bexp nb r) with
          | ERROR_BOOL e -> 
            (raise (Error ("runtime error in \"while\" loop at " 
	                   ^ (string_of_label l))))
          | BOOLEAN true -> (l'', r)
          | BOOLEAN false -> (raise (Error "WHILE test incoherence")))
      else if (l = (after c')) then (l', r)
      else if (incom l c') then
        (trans c' (l, r))
      else (raise (Error "WHILE incoherence")))
and transseq s (l, r) = match s with
| []   -> raise (Error "empty SEQ incoherence")
| [c]  -> if (incom l c) then
            (trans c (l, r)) 
          else (raise (Error "SEQ incoherence"))
| h::t -> if (l = (after h)) then (transseq t (l, r))
          else if (incom l h) then (trans h (l, r)) 
          else (transseq t (l, r))

