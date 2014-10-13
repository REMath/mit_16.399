(* caexp.mli *)
open Abstract_Syntax   
open Cvalues
open Cenv
(* evaluation of arithmetic operations *)
val c_aexp : aexp -> Cenv.t -> Cvalues.t
