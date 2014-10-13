(* fixpoint.mli *)
open Cenv
(* lfp x c f : iterative computation of the c-least fixpoint of f *)
(* c-greater than or equal to the prefixpoint x (f(x) >= x)       *)
val lfp : t -> (t -> t -> bool) -> (t -> t) -> t