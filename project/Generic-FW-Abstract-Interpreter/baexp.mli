(* baexp.mli *)
open Abstract_Syntax    
open Avalues
open Aenv
val b_aexp : aexp ->  Aenv.t -> Avalues.t -> Aenv.t
