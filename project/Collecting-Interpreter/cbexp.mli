(* cbexp.mli *)
open Abstract_Syntax
open Cvalues
open Cenv
(* evaluation of boolean operations *)
val c_bexp : bexp -> Cenv.t -> Cenv.t
