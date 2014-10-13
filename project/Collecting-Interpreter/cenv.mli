(* cenv.mli *)
open Abstract_Syntax
open Cvalues
open Env
(* set of environments *)
type elt = Env.env
and t
(* infimum *)
val bot : unit -> t
(* check for infimum *)
val is_bot : t -> bool
(* uninitialization *)
val initerr : unit -> t
(* supremum *)
val top : unit -> 'a
(* copy *)
val copy : t -> t
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
val get : t -> variable -> Cvalues.t
(* r[X <- i] = {e[X <- i] | e in r } *)
val set_elem : t -> variable -> Values.machine_int -> t
(* r[X <- v] = {e[X <- i] | e in r /\ i in v}    *)
val set : t -> variable -> Cvalues.t -> t
(* collecting semantics of assignment                             *)
(* f_ASSIGN x f r =  {e[x <- i] | e in r /\ i in f({e}) cap I }   *)
val f_ASSIGN : variable -> (t -> Cvalues.t) -> t -> t
(* collecting semantics of boolean expressions                    *)
(* f_EQ f g r =                                                   *)
(*   {e in R | exists v1 in f({e}) cap I: exists v2 in g({e}) cap *)
(*             I: v1 = v2 }                                       *)
val f_EQ : (t -> Cvalues.t) -> (t -> Cvalues.t) -> t -> t
(* f_LT f g r =                                                   *)
(*   {e in R | exists v1 in f({e}) cap I: exists v2 in g({e}) cap *)
(*             I: v1 < v2 }                                       *)
val f_LT : (t -> Cvalues.t) -> (t -> Cvalues.t) -> t -> t
