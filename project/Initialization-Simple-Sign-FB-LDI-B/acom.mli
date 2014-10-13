(* acom.mli *)
open Abstract_Syntax
open Labels
open Aenv
(* forward abstract interpretation of commands *)
val acom : com -> Aenv.t -> label -> Aenv.t
