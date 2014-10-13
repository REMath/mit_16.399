(* variables.mli *)
open Symbol_Table    
val number_of_variables : unit -> int
val for_all_variables   : (variable -> 'a) -> unit
val print_variable      : variable -> unit
val print_map_variables : (variable -> unit) -> unit
