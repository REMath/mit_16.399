(* ccom.mli *)
open Abstract_Syntax
open Labels
open Cenv
(* forward collecting semantics of commands *)
val ccom : com -> Cenv.t -> label -> Cenv.t
