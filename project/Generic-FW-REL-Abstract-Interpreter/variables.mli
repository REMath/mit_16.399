(* variables.mli *)
open Symbol_Table    
type variable = Symbol_Table.variable
val number_of_variables : unit -> int
val for_all_variables : (variable -> 'a) -> unit
val print_variable : variable -> unit
val map_variables : (variable -> unit) -> (variable -> unit) -> unit
val string_of_variable : variable -> string
