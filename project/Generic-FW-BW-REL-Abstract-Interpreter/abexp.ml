(* abexp.ml *)
open Linear_Syntax
open Aenv
(* abstract interpretation of linearized boolean operations *)
exception Error of string
let rec a_bexp b r =
  match b with
  | RANDOM_BEXP -> r
  | LTRUE       -> r
  | LFALSE      -> (Aenv.bot ())
  | (LGE a)     -> (Aenv.f_LGE a r)
  | (LEQ a)     -> (Aenv.f_LEQ a r)
  | (LAND l)    -> let rec andlist l = match l with
	            | []     -> (raise (Error "empty LAND incoherence"))
	            | b'::[] -> a_bexp b' r
	            | b'::l' -> Aenv.meet (a_bexp b' r) (andlist l')
	           in andlist l
  | (LOR l)     -> let rec orlist l = match l with
	            | []     -> (raise (Error "empty LOR incoherence"))
	            | b'::[] -> a_bexp b' r
	            | b'::l' -> Aenv.join (a_bexp b' r) (orlist l')
	           in orlist l

