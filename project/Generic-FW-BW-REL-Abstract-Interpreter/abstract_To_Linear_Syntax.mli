(* abstract_To_Linear_Syntax.mli *)
open Abstract_Syntax   
open Linear_Syntax
(* Linearization of commands *)
val linearize_com : com -> lcom
