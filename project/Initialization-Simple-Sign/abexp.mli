(* abexp.mli *)
open Abstract_Syntax
open Avalues
open Aenv
(* abstract interpretation of boolean operations *)
val a_bexp : bexp -> Aenv.t -> Aenv.t
