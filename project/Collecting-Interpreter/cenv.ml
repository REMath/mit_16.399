(* cenv.ml *)
open Variables
open Values
open Cvalues
open Env
(* order on values *)
let compare_values v1 v2 = match v1, v2 with
   | (ERROR_NAT INITIALIZATION), (ERROR_NAT INITIALIZATION) -> 0
   | (ERROR_NAT INITIALIZATION), (ERROR_NAT ARITHMETIC) -> -1
   | (ERROR_NAT ARITHMETIC), (ERROR_NAT INITIALIZATION) -> 1
   | (ERROR_NAT ARITHMETIC), (ERROR_NAT ARITHMETIC) -> 0
   | (ERROR_NAT e), (NAT i) -> -1
   | (NAT i), (ERROR_NAT e) -> 1
   | (NAT i), (NAT j) -> if (i<j) then -1 else if (i=j) then 0 else 1
(* ordered set of environments *)
exception Found of int
include Set.Make
   (struct
       type t = env
       (* order on environments *)
       let compare r1 r2 =
         try
           for i = 0 to ((number_of_variables ()) - 1) do
             let c = compare_values (get r1 i) (get r2 i) in
               if c != 0 then   raise (Found c)
           done;
           0
         with Found c -> c
   end)
(* infimum *)
let bot () = empty 
(* check for infimum *)
let is_bot = is_empty 
(* uninitialization *)
let initerr () = singleton (Env.initerr ())
(* supremum *)
exception ErrorCenv of string
let top () = raise (ErrorCenv "top not implemented")
(* copy *)
let copy s = s (* implementation without side-effects *) 
(* least upper bound *)
let join = union 
(* greatest lower bound *)
let meet = inter 
(* approximation ordering *)
let leq = subset
(* equality *)
let eq = equal
(* printing *)
let print r =
   print_string "{ ";
   let pe e = (print_string "[ ";print_env e;print_string "] ") in
   iter  pe r;
   print_string "}"
(* r(X) = {e(X) | e in r}               *)
(* val get : t -> variable -> Cvalues.t *)
let get r x =
  let f e s = Cvalues.add (Env.get e x) s in
    fold f r (Cvalues.bot ())
(* r[X <- i] = {e[X <- i] | e in r }                       *)
(* val set_elem : t -> variable -> Values.machine_int -> t *)
let set_elem r x i  =
  let f e s = add (let e' = (Env.copy e) in Env.set e' x i; e') s in 
    fold f r empty
(* r[X <- v] = {e[X <- i] | e in r /\ i in v}    *)
(* val set : t -> variable -> Cvalues.t -> t *)
let set r x v = 
    let f i s = union (set_elem r x i) s in
	   Cvalues.fold f v empty
(* f_ASSIGN x f r =  {e[x <- i] | e in r /\ i in f({e}) cap I } *)
let f_ASSIGN x f r =
 let assign e s = 
   let a i s' = 
	  match i with
     | ERROR_NAT _ -> s'
     | NAT _ -> add (let e' = (Env.copy e) in (Env.set e' x i; e')) s'
   in  Cvalues.fold a (f (singleton e)) s
 in fold assign r empty 
(* cmp c f g r =                                              *)
(*   {e in R | exists v1 in f({e}) cap I: exists v2 in g({e}) *)
(*             cap I: v1 c v2 }                               *)
(* val cmp : (elt -> elt -> Values.machine_bool) -> (t -> t)  *)
(*                                      -> (t -> t) -> t -> t *)
exception Found
let cmp c f g r =
  let isFound i j = 
    match (c i j) with
    | ERROR_BOOL _  -> ()
    | BOOLEAN false -> ()
    | BOOLEAN true  -> raise Found
  in let ok e = 
    let s1 = (f (singleton e))  and s2 = (g (singleton e)) 
      in (try
        let tests2 j = 
          (let tests1 i = isFound i j in Cvalues.iter tests1 s1)
        in Cvalues.iter tests2 s2;
           false
        with Found -> true)
    in filter ok r
(* f_EQ f g r =                                               *)
(*   {e in R | exists v1 in f({e}) cap I: exists v2 in g({e}) *)
(*                                           cap I: v1 = v2 } *)
let f_EQ f g r = cmp machine_eq f g r
(* f_LT f g r =                                               *)
(*   {e in R | exists v1 in f({e}) cap I: exists v2 in g({e}) *)
(*                                           cap I: v1 < v2 } *)
let f_LT f g r = cmp machine_lt f g r