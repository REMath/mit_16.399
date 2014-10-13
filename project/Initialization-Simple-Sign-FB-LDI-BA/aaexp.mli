(* aaexp.mli *)
open Abstract_Syntax   
open Avalues
open Aenv
(* evaluation of arithmetic operations *)
val a_aexp : aexp -> Aenv.t -> Avalues.t
