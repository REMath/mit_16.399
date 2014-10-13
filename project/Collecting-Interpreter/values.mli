(* values.mli *)
type error_type = INITIALIZATION | ARITHMETIC
(* machine integers *)
type machine_int = ERROR_NAT of error_type | NAT of int
val machine_int_of_string : string -> machine_int
val machine_unary_random : unit -> machine_int
val machine_unary_plus : machine_int -> machine_int
val machine_unary_minus : machine_int -> machine_int
val machine_binary_plus : machine_int -> machine_int -> machine_int
val machine_binary_minus : machine_int -> machine_int -> machine_int
val machine_binary_times : machine_int -> machine_int -> machine_int
val machine_binary_div : machine_int -> machine_int -> machine_int
val machine_binary_mod : machine_int -> machine_int -> machine_int

(* machine booleans *)
type machine_bool = ERROR_BOOL of error_type | BOOLEAN of bool
val machine_eq : machine_int -> machine_int -> machine_bool 
val machine_lt : machine_int -> machine_int -> machine_bool 
val machine_and : machine_bool -> machine_bool -> machine_bool
val machine_or : machine_bool -> machine_bool -> machine_bool

(* printing *)
val print_machine_int : machine_int -> unit