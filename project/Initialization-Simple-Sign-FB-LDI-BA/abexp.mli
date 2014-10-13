(* abexp.mli *)
open Abstract_Syntax
(* abstract interpretation of boolean operations *)
val a_bexp : bexp -> Aenv.t -> Aenv.t
