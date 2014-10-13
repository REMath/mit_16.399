(* acom.mli *)
open Linear_Syntax
open Aenv
(* forward abstract interpretation of commands *)
val acom : lcom -> Aenv.t -> label -> Aenv.t
