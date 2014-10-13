(* avalues.mli *)
(* abstraction of sets of machine integers by initialization and simple sign *)
type t
(* bot () = alpha({}) *)
val bot : unit -> t
(* isbotempty () = gamma(bot ()) = {} *)
val isbotempty : unit -> bool
(* initerr () = alpha({_O_i}) *)
val initerr : unit -> t
(* top () = alpha({_O_i, _O_a} U [min_int,max_int]) *)
val top : unit -> t
(* join p q = alpha(gamma(p) U gamma(q)) *)
val join : t -> t -> t
(* meet p q = alpha(gamma(p) cap gamma(q)) *)
val meet : t -> t -> t
(* leq p q = gamma(p) subseteq gamma(q) *)
val leq : t -> t -> bool
(* eq p q = gamma(p) = gamma(q) *)
val eq : t -> t -> bool
(* in_errors p = gamma(p) subseteq {_O_i, _O_a} *)
val in_errors : t -> bool
(* printing *)
val print : t -> unit
(* forward abstract semantics of arithmetic expressions *)
(* f_NAT s = alpha({(machine_int_of_string s)})        *)
val f_NAT : string -> t
(* f_RANDOM () = alpha([min_int, max_int]) *)
val f_RANDOM : unit -> t
(* f_UMINUS a = alpha({Ê(machine_unary_minus x) | x \in gamma(a)} }) *)
val f_UMINUS : t -> t
(* f_UPLUS a = alpha(gamma(a)) *)
val f_UPLUS : t -> t
(* f_BINARITH a b = alpha({Ê(machine_binary_binarith i j)| }) | i in gamma(a) /\ j \in gamma(b)} *)
val f_PLUS : t -> t -> t
val f_MINUS : t -> t -> t
val f_TIMES : t -> t -> t
val f_DIV : t -> t -> t
val f_MOD : t -> t -> t
(* forward abstract semantics of boolean expressions                 *)
(* f_LT p q = exists i in gamma(p) cap [min_int,max_int]:            *)
(*        exists j in gamma(q) cap [min_int,max_int]: machine_eq i j *)
val f_EQ : t -> t -> bool
(* f_LT p q = exists i in gamma(p) cap [min_int,max_int]:            *)
(*        exists j in gamma(q) cap [min_int,max_int]: machine_lt i j *)
val f_LT : t -> t -> bool
(* widening/narrowing *)
val widen : t -> t -> t
val narrow : t -> t -> t
(* backward abstract semantics of arithmetic expressions                     *)
(* b_NAT s v = (machine_int_of_string s) in gamma(v) cap [min_int, max_int]? *)
val b_NAT    : string -> t -> bool
(* b_RANDOM p = gamma(p) cap I <> emptyset *)
val b_RANDOM : t -> bool
(* b_UOP q p = alpha({i in gamma(q) | UOP(i) \in gamma(p) cap [min_int, max_int]}) *)
val b_UMINUS : t -> t -> t
val b_UPLUS  : t -> t -> t
(* b_BOP q1 q2 p = alpha2({<i1,i2>  in gamma2(<q1,q2>) | BOP(i1, i2) \in gamma(p) cap I}) *)
val b_PLUS   : t -> t -> t -> t * t
val b_MINUS  : t -> t -> t -> t * t
val b_TIMES  : t -> t -> t -> t * t
val b_DIV    : t -> t -> t -> t * t
val b_MOD    : t -> t -> t -> t * t
(* backward abstract interpretation of boolean expressions *)
(* a_EQ p1 p2 = let p = p1 cap p2 cap I in <p, p> *)
val a_EQ     : t -> t -> t * t
(* a_LT p1 p2 = alpha({<i1, i2> | i1 in gamma(p1) cap [min_int, max_int] /\            *)
(*                                i2 in gamma(p1) cap [min_int, max_int] /\ i1 <= i2}) *)
val a_LT     : t -> t -> t * t