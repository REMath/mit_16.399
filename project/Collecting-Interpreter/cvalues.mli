(* cvalues.mli *)
open Values
(* set of machine integers *)
type elt = machine_int
and t
val add : elt -> t -> t
val singleton : elt -> t
val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
val iter : (elt -> unit) -> t -> unit
val bot : unit -> t
val isbotempty : unit -> bool
val initerr : unit -> t
val top : unit -> 'a
val join : t -> t -> t
val meet : t -> t -> t
val leq : t -> t -> bool
val eq : t -> t -> bool
val in_errors : t -> bool
val print : t -> unit
(* forward collecting semantics of arithmetic expressions *)
val f_NAT : string -> t
val f_RANDOM : unit -> t
val f_UMINUS : t -> t
val f_UPLUS : t -> t
val f_PLUS : t -> t -> t
val f_MINUS : t -> t -> t
val f_TIMES : t -> t -> t
val f_DIV : t -> t -> t
val f_MOD : t -> t -> t
