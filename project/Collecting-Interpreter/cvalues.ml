(* cvalues.ml *)
open Values
(* ordered set of machine integers *)
include Set.Make 
 (struct
   type t = machine_int
   (* order on values *)
   let compare v1 v2 = match v1, v2 with
   | (ERROR_NAT INITIALIZATION), (ERROR_NAT INITIALIZATION) -> 0
   | (ERROR_NAT INITIALIZATION), (ERROR_NAT ARITHMETIC) -> -1
   | (ERROR_NAT ARITHMETIC), (ERROR_NAT INITIALIZATION) -> 1
   | (ERROR_NAT ARITHMETIC), (ERROR_NAT ARITHMETIC) -> 0
   | (ERROR_NAT e), (NAT i) -> -1
   | (NAT i), (ERROR_NAT e) -> 1
   | (NAT i), (NAT j) -> if (i<j) then -1 else if (i=j) then 0 else 1
  end)
(* infimum *)
let bot () = empty 
(* bottom is emptyset? *)
let isbotempty () = true
(* uninitialization *)
let initerr () = singleton (ERROR_NAT INITIALIZATION)
(* supremum *)
exception ErrorCvalues of string
let top () = raise (ErrorCvalues "top not implemented")
(* least upper bound *)
let join = union 
(* greatest lower bound *)
let meet = inter 
(* approximation ordering *)
let leq = subset
(* equality *)
let eq = equal
(* included in errors? *)
let in_errors v = 
  let iserror i = match i with
    | (ERROR_NAT e) -> true
    | (NAT j) -> false
  in for_all iserror v
(* printing *)
let print v =
   let printelement e =
      print_machine_int e;
      print_string " "
   in
      print_string "{ ";
     iter printelement v;
      print_string " }"
(* image u s = { u(x) | x in s } *)
let image u s =
    let f e s' = add (u e) s' in
      fold  f s empty
(* set_bin b s1 s2 = { b(x,y) | x in s1 /\ y in s2 } *)
let set_bin b s1 s2 =
    let f a2 s = 
      let g a1 s = add (b a1 a2) s
      in  fold g s1 empty
    in
       fold f s2 empty
(* forward collecting semantics of arithmetic expressions *)
let f_NAT s = 
   singleton (machine_int_of_string s)
let r1 = (machine_unary_random ())
let r2 = (machine_unary_random ())
let r3 = (machine_unary_random ())
let f_RANDOM () = 
  (* should be the set of all possible values! *)
  add r1 (add r2 (singleton r3))
let f_UMINUS a = 
   image machine_unary_minus a
let f_UPLUS a =
   image machine_unary_plus a
let f_PLUS a1 a2 = set_bin machine_binary_plus a1 a2
let f_MINUS a1 a2 = set_bin machine_binary_minus a1 a2
let f_TIMES a1 a2 = set_bin machine_binary_times a1 a2
let f_DIV a1 a2 = set_bin machine_binary_div a1 a2
let f_MOD a1 a2 = set_bin machine_binary_mod a1 a2
