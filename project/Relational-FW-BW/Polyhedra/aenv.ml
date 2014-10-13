(* aenv.ml *)
open Linear_Syntax
open Variables
type lattice = BOT | TOP
type t = 
  NULL of lattice  (* in absence of any variable, dimension = 0 *)
| POLY of Poly.t   (* must be of dimension > 0                  *)
exception PolyError of string
(* relational library initialization *)
let maxdims = 10000
let maxrows = 100
let init () = (Polka.initialize false maxdims maxrows; 
               Polka.strict := false)
(* relational library exit (* print statistics *) *)
let quit () = Polka.finalize ()
(* infimum *)
let bot () = match (number_of_variables ()) with
 | 0 -> NULL BOT
 | n -> if (n < 0) then
     raise (PolyError "negative number of variables (bot)")
   else if (n > maxdims) then
     raise (PolyError "too many variables (bot)")
   else
     POLY (Poly.empty n) (* 1 <= n <= polka_maxcolumns-polka_dec *)
(* check for infimum *)
let is_bot r = match r with
 | NULL BOT -> true
 | NULL TOP -> false
 | POLY p -> (Poly.is_equal p (Poly.empty (number_of_variables ())))
(* uninitialization *)
let initerr () = match (number_of_variables ()) with
 | 0 -> NULL TOP
 | n -> if (n < 0) then
     raise (PolyError "negative number of variables (initerr)")
   else if (n > maxdims) then
     raise (PolyError "too many variables (initerr)")
   else
     POLY (Poly.universe n)
(* supremum *)
let top () =  match (number_of_variables ()) with
 | 0 -> NULL TOP
 | n -> if (n < 0) then
     raise (PolyError "negative number of variables (top)")
   else if (n > maxdims) then
     raise (PolyError "too many variables (top)")
   else
     POLY (Poly.universe n)
(* least upper bound *)
let ljoin l1 l2 = match (l1, l2) with
 | BOT, _ -> l2
 | _, BOT -> l1
 | _, _   -> TOP
let join r1 r2 = match (r1, r2) with
 | NULL l1, NULL l2 -> (NULL (ljoin l1 l2))
 | POLY p1, POLY p2 -> (POLY (Poly.union p1 p2))
 | _, _ -> raise (PolyError "join")
(* greatest lower bound *)
let lmeet l1 l2 = match (l1, l2) with
 | TOP, _ -> l2
 | _, TOP -> l1
 | _, _   -> BOT
let meet r1 r2 = match (r1, r2) with
 | NULL l1, NULL l2 -> (NULL (lmeet l1 l2))
 | POLY p1, POLY p2 -> (POLY (Poly.inter p1 p2))
 | _, _ -> raise (PolyError "meet")
(* approximation ordering *)
let lleq l1 l2 = match (l1, l2) with
 | BOT, _   -> true
 | _, TOP   -> true
 | TOP, BOT -> false
let leq r1 r2 = match (r1, r2) with
 | NULL l1, NULL l2 -> (lleq l1 l2)
 | POLY p1, POLY p2 -> (Poly.is_included_in p1 p2)
 | _, _ -> raise (PolyError "leq")
(* equality *)
let eq r1 r2 = match (r1, r2) with
 | NULL l1, NULL l2 -> (l1 = l2)
 | POLY p1, POLY p2 -> (Poly.is_equal p1 p2)
 | _, _ -> raise (PolyError "eq")
(* printing *)
let print r = match r with
 | NULL BOT -> (print_string "{ _|_ }\n")
 | NULL TOP -> (print_string "{ T }\n")
 | POLY p ->
   (Poly.minimize p; (* to get the constraints and generators of p *)
    Poly.print_constraints string_of_variable Format.std_formatter p;
    Format.pp_print_newline Format.std_formatter ())
(* convert a0.v0+...+an-1.vn-1+an where n = (number_of_variables ()) into *)
(* vector [1,an,a0,...,an-1].                                             *)
let vector_of_lin_expr a =
  let v = Vector.make ((number_of_variables ()) + 2) in
    (Vector.set v 0 1;
     Vector.set v 1 (a.(number_of_variables ()));
     for i = 0 to ((number_of_variables ()) - 1) do
       Vector.set v (i+2) a.(i)
     done;
     (* 
     Vector._print v;
     Vector.print_constraint string_of_variable Format.std_formatter v;
     Format.pp_print_newline Format.std_formatter ();
     *)
     v)
(* f_ASSIGN x f r =  {e[x <- i] | e in r /\ i in f({e}) cap I } *)
let f_ASSIGN x f r = 
  match r with
 | NULL _ -> r
 | POLY p -> (match f with
 | RANDOM_AEXP -> 
    let d = [|{ Polka.pos = x; Polka.nbdims = 1 }|] in
      (POLY (Poly.add_dims_and_embed_multi (Poly.del_dims_multi p d) d))
 | LINEAR_AEXP a -> 
      (POLY (Poly.assign_var p x (vector_of_lin_expr a))))
(* b_ASSIGN x f r =  {e | exists i in f({e}) cap I: e[x <- i] in r } *)
let b_ASSIGN x f r = 
  match r with
 | NULL _ -> r
 | POLY p -> POLY (match f with
 | RANDOM_AEXP -> 
     let d = [|{ Polka.pos = x; Polka.nbdims = 1 }|] in
        (Poly.add_dims_and_embed_multi (Poly.del_dims_multi p d) d)
 | LINEAR_AEXP a -> 
        (Poly.minimize p; (* <- to get around a bug of Polka 2.0.2   *)
                          (* in substitute_var                       *)
         Poly.substitute_var p x (vector_of_lin_expr a)))
(* f_LGE a r = {e in r | a0.v0+...+an-1.vn-1+an >= 0} *)
let f_LGE a r = 
  match r with
 | NULL _ -> r
 | POLY p -> POLY (Poly.add_constraint p (vector_of_lin_expr a))
(* f_LEQ a r = {e in r | a0.v0+...+an-1.vn-1+an = 0} *)
let minus = Array.map (fun x -> (- x))
let f_LEQ a r = meet (f_LGE a r) (f_LGE (minus a) r)
(* widening *)
let widen r1 r2 = match (r1, r2) with
 | NULL l1, NULL l2 -> (NULL (ljoin l1 l2))
 | POLY p1, POLY p2 -> (POLY (Poly.widening p1 p2))
 | _, _ -> raise (PolyError "widen")
(* narrowing *)
(* let narrow a b = a (* does not ensure termination *) *)
let narrow a b = b (* less precise but does ensure termination *)
