(* bexp.mli *)
open Abstract_Syntax
open Values
open Env
(* evaluation of boolean operations *)
val eval_bexp : bexp -> env -> machine_bool
