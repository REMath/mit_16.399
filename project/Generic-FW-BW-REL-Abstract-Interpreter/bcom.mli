(* bcom.mli *)
open Linear_Syntax
open Aenv
(* backward abstract interpretation of commands *)
val bcom : lcom -> Aenv.t -> label -> Aenv.t
