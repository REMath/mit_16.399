(* fixpoint.mli *)
open Aenv
(* lfp x c w n f : iterative computation of a c-postfixpoint of f *)
(* c-greater than or equal to the prefixpoint x (x <= f(x)) with  *)
(* widening w and narrowing n                                     *)
val lfp : t -> (t -> t -> bool) ->
    (t -> t -> t) -> (t -> t -> t) -> (t -> t) -> t
(* gfp x c n f : iterative computation of a c-postfixpoint of f   *)
(* c-less than or equal to the postfixpoint x (f(x) <= x) with    *)
(* narrowing n                                                    *)
val gfp : t -> (t -> t -> bool) -> (t -> t -> t) -> (t -> t) -> t