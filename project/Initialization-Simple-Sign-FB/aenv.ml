(* aenv.ml *)
open Variables
open Avalues
open Array
type t = Avalues.t array
(* infimum *)
let bot () = Array.create (number_of_variables ()) (Avalues.bot ()) 
(* check for infimum *)
let is_bot r = 
   let f x v = x or (Avalues.eq v (Avalues.bot ())) in
      Array.fold_left f false r
(* uninitialization *)
let initerr () = 
  Array.create (number_of_variables ()) (Avalues.initerr ())
(* supremum *)
let top () = Array.create (number_of_variables ()) (Avalues.top ())
(* copy *)
let copy = copy (* implementation without side-effects *) 
(* least upper bound *)
let join r1 r2 =
   let f i v = (Avalues.join (get r1 i) v) in
      mapi f r2
(* greatest lower bound *)
let meet r1 r2 =
    let f i v = (Avalues.meet (get r1 i) v) in
        mapi f r2
(* approximation ordering *)
exception NotTrue
let leq r1 r2 =
   try 
      let f x = 
         if not (Avalues.leq (get r1 x) (get r2 x)) then
            raise NotTrue
         in for_all_variables f;
         true
   with
      NotTrue -> false
(* equality *)
let eq r1 r2 =
   try 
      let f i = 
         if not (Avalues.eq (get r1 i) (get r2 i)) then
            raise NotTrue
      in for_all_variables f;
      true
   with
      NotTrue -> false
(* printing *)
let print r =
   let p v = Avalues.print (get r v) in
      print_map_variables p
(* r(X) = {e(X) | eo in r}                    *)
(* val get : t -> variable -> Cvalues.t       *)
let get r x = (get r x)
(* r[X <- v] = {e[X <- i] | e in r /\ i in v} *)
(* val set : t -> variable -> Cvalues.t -> t  *)
let set r x v = 
   if (Avalues.eq v (Avalues.bot ())) & (Avalues.isbotempty ()) then 
      (bot ()) (* reduce *)
   else
      (let r' = copy r in (set r' x v; r'))
(* f_ASSIGN x f r =  {e[x <- i] | e in r /\ i in f({e}) cap I }      *)
let f_ASSIGN x f r = set r x (Avalues.meet (f r) (Avalues.f_RANDOM ()))
(* f_EQ f g r =                                                      *)
(*   {e in R | exists v1 in f({e}) cap I: exists v2 in g({e}) cap I: *)
(*             v1 = v2 }                                             *)
let f_EQ f g r = if (Avalues.f_EQ (f r) (g r)) then r else (bot ())
(* f_LT f g r =                                                      *)
(*   {e in R | exists v1 in f({e}) cap I: exists v2 in g({e}) cap I: *)
(*             v1 < v2 }                                             *)
let f_LT f g r = if (Avalues.f_LT (f r) (g r)) then r else (bot ())
