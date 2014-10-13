(* aenv.mli *)
open Linear_Syntax
open Array
open Variables
(* set of environments *)
type t
(* relational library initialization *)
val init : unit -> unit
(* relational library exit *)
val quit : unit -> unit
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
(* forward collecting semantics of assignment                   *)
(* f_ASSIGN x f r =  {e[x <- i] | e in r /\ i in f({e}) cap I } *)
val f_ASSIGN : variable -> laexp -> t -> t
(* backward collecting semantics of assignment                       *)
(* b_ASSIGN x f r =  {e | exists i in f({e}) cap I: e[x <- i] in r } *)
val b_ASSIGN : variable -> laexp -> t -> t
(* collecting semantics of boolean expressions                  *)
(* f_LGE a r = {e in r | a0.v0+...+an-1.vn-1 >= an              *)
val f_LGE : (int array) -> t -> t
(* f_LEQ a r = {e in r | a0.v0+...+an-1.vn-1 = an               *)
val f_LEQ : (int array) -> t -> t
(* convergence acceleration *)
(* widening *)
val widen : t -> t -> t
(* narrowing *)
val narrow : t -> t -> t
