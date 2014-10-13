(* avalues.mli *)
(* abstraction of sets of machine integers by initialization *)
(* and simple sign                                           Ä*)
type t
val bot : unit -> t
val isbotempty : unit -> bool
val initerr : unit -> t
val top : unit -> t
val join : t -> t -> t
val meet : t -> t -> t
val leq : t -> t -> bool
val eq : t -> t -> bool
val in_errors : t -> bool
val print : t -> unit
(* forward abstract semantics of arithmetic expressions *)
val f_NAT : string -> t
val f_RANDOM : unit -> t
val f_UMINUS : t -> t
val f_UPLUS : t -> t
val f_PLUS : t -> t -> t
val f_MINUS : t -> t -> t
val f_TIMES : t -> t -> t
val f_DIV : t -> t -> t
val f_MOD : t -> t -> t
(* forward abstract semantics of boolean expressions *)
val f_EQ : t -> t -> bool
val f_LT : t -> t -> bool
(* backward abstract interpretation of arithmetic expressions *)
val b_NAT    : string -> t -> bool
val b_RANDOM : t -> bool
val b_UMINUS : t -> t -> t
val b_UPLUS  : t -> t -> t
val b_PLUS   : t -> t -> t -> t * t
val b_MINUS  : t -> t -> t -> t * t
val b_TIMES  : t -> t -> t -> t * t
val b_DIV    : t -> t -> t -> t * t
val b_MOD    : t -> t -> t -> t * t
(* abstract interpretation of boolean expressions *)
val a_EQ     : t -> t -> t * t
val a_LT     : t -> t -> t * t
