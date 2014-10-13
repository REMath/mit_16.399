(* aexp.mli *)
open Abstract_Syntax   
open Values
open Env
(* evaluation of arithmetic operations *)
val eval_aexp : aexp ->  env -> machine_int 
