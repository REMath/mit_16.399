(* symbol_Table.mli *)
(* initialization of the symbol table to empty               *)
val init_symb_table     : unit -> unit
(* variables are represented by their rank                   *)
type variable = int
(* if absent, add variable with given string to symbol table *)
(* and return its rank                                       *)
val add_symb_table      : string -> variable
(* number of variables in the symbol table                   *)
val number_of_variables : unit -> int
(* string of given variable in the symbol table              *)
val string_of_variable  : variable -> string
(* print given program variable                              *)
val print_variable      : variable -> unit
(* (for_all_variables f) iterates application of f to all    *)
(* variables in the symbol table                             *)
val for_all_variables   : (variable -> 'a) -> unit
(* (print_map_variables f) prints {...; vi : f vi;...} for   *)
(* all variables vi in the symbol table                      *)
val print_map_variables : (variable -> unit) -> unit
(* map_variables p = (p v0) (p v1) ... (p vn-2) (p vn-1)     *)
(* where v0, ..., vn-1 are the n >= 0 program variables      *)
val map_variables       : (variable -> unit) -> unit
(* map2_variables p q =                                      *)
(*   (p v0) (q v1) (p v1) ... (p vn-2) (q vn-1) (p vn-1)     *)
(* where v0, ..., vn-1 are the n >= 0 program variables      *)
val map2_variables      : (variable -> unit) -> 
                                     (variable -> unit) -> unit
