(* aenv.mli *)
open Abstract_Syntax
open Avalues
(* set of environments *)
type t
(* infimum *)
val bot : unit -> t
(* check for infimum *)
val is_bot : t -> bool
(* uninitialization *)
val initerr : unit -> t
(* supremum *)
val top : unit -> t
(* least upper bound *)
val join : t -> t -> t
(* greatest lower bound *)
val meet : t -> t -> t
(* approximation ordering *)
val leq : t -> t -> bool
(* equality *)
val eq : t -> t -> bool
(* printing *)
val print : t -> unit
(* r(X) = {e(X) | X in r} *)
val get : t -> variable -> Avalues.t
(* r[X <- v] = {e[X <- i] | e in r /\ i in v}    *)
val set : t -> variable -> Avalues.t -> t
(* abstract semantics of assignment                                          *)
(* f_ASSIGN x f r =  {e[x <- i] | e in r /\ i in f({e}) cap I }                *)
val f_ASSIGN : variable -> (t -> Avalues.t) -> t -> t
(* collecting semantics of boolean expressions                                 *)
(* f_EQ f g r =                                                                *)
(*   {e in R | exists v1 in f({e}) cap I: exists v2 in g({e}) cap I: v1 = v2 } *)
val f_EQ : (t -> Avalues.t) -> (t -> Avalues.t) -> t -> t
(* f_LT f g r =                                                                *)
(*   {e in R | exists v1 in f({e}) cap I: exists v2 in g({e}) cap I: v1 < v2 } *)
val f_LT : (t -> Avalues.t) -> (t -> Avalues.t) -> t -> t
(* widening *)
val widen : t -> t -> t
(* narrowing *)
val narrow : t -> t -> t
