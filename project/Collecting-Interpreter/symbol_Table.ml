(* symbol_Table.ml *)
open List
type variable = int

(* global list of program variable strings. A variable *)
(* is the rank of its string in symb_table.            *)
let symb_table = ref []

(* initialization of the symbol table to empty *)
let init_symb_table () = 
  symb_table := []

(* variable v with string i in symbol table *)
exception Rank of int
exception NotFound of int
let variable_of_string i =
  let p = ref !symb_table
  and rv = ref 0 in
    try
      while !p <> [] do
        if (hd !p) = i then
          raise (Rank !rv)
        else 
          (p := tl !p;
           rv := !rv + 1)
      done;
      raise (NotFound !rv)
    with
      (Rank v) -> v

(* string of variable v in symbol table *)
exception Error_string_of_variable of string    
let string_of_variable v =
 let p = ref !symb_table in
  for k = 0 to (v - 1) do
    if !p = [] then
      raise (Error_string_of_variable "too large")
    else
      p := tl !p
  done;
  if !p = [] then
    raise (Error_string_of_variable "not found")
  else
    hd !p

(* if absent, add variable with string i to symbol table and *)
(* return its rank                                           *)
let add_symb_table i =
 try 
   variable_of_string i
 with
   NotFound v -> (symb_table := (!symb_table) @ [i]; v)

(* number of variables in the symbol table *)
let number_of_variables () = length !symb_table    

(* iterate application of f to all variables *)
let for_all_variables f =
  for i = 0 to ((number_of_variables ()) - 1) do
    f i
  done

(* print program variables *)
exception Error_print_variable of string    
let print_variable v =
  if v < (length !symb_table) then
    print_string (string_of_variable v)
  else
    raise (Error_print_variable ( (string_of_int v) ^ " not in list"))

(* print {...; vi : f vi;...} for all variables vi *)
let print_map_variables p =
  if (number_of_variables ()) = 0 then
    print_string "{}"
  else
    (print_string "{ ";
     for i = 0 to ((number_of_variables ()) - 2) do
       print_variable i; print_string ":"; p i; 
       print_string "; "
     done;
     let lv = ((number_of_variables ()) - 1) in
       (print_variable lv; print_string ":"; p lv);
     print_string " }")

(* map_variables p = (p v0) (p v1) ... (p vn-2) (p vn-1) *)
(* where v0, ..., vn-1 are the n >= 0 program variables  *)
let map_variables p =
  if (number_of_variables ()) > 0 then
    (for v = 0 to ((number_of_variables ()) - 1) do
       p v
     done)
  else
    ()
(* map2_variables p q =                                     *)
(*     (p v0) (q v1) (p v1) ... (p vn-2) (q vn-1) (p vn-1)  *)
(* where v0, ..., vn-1 are the n >= 0 program variables     *)
let map2_variables p q =
  if (number_of_variables ()) > 0 then
    (p 0;
     for v = 1 to ((number_of_variables ()) - 1) do
       q v;
       p v
     done)
  else
    ()
