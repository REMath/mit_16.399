(* symbol_Table.mli *)
type variable = int
(* symbol table *)
val init_symb_table : unit -> unit
val add_symb_table  : string -> variable
(* variables *)
val number_of_variables : unit -> int
val for_all_variables   : (variable -> 'a) -> unit
val print_variable      : variable -> unit
val map_variables       : (variable -> unit) -> (variable -> unit) -> unit
val string_of_variable  : variable -> string
