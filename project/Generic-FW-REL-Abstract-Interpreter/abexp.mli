(* abexp.mli *)
open Linear_Syntax
open Aenv
(* abstract interpretation of boolean operations *)
val a_bexp : lbexp -> Aenv.t -> Aenv.t
