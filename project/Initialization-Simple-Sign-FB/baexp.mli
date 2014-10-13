(* baexp.mli *)
open Abstract_Syntax   
open Avalues
open Aenv
(* backward evaluation of arithmetic operations *)
val b_aexp : aexp -> Aenv.t -> Avalues.t -> Aenv.t
