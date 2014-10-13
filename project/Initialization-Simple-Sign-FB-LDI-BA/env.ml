(* env.ml *)
open Variables
open Values
open Array
type env = machine_int array
(* uninitialization *)
let initerr () = make (number_of_variables ()) (ERROR_NAT INITIALIZATION)
(* copy *)
let copy  r = (copy r)
(* substitution *)
let get r x = (get r x)
let set r x v = (set r x v)
let print_env r =
  let printithvar i v =
    (print_variable i;
	  print_string " = "; 
	  print_machine_int v;
	  print_string "; ")
  in
    iteri printithvar r
