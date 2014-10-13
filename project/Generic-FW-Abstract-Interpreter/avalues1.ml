(* avalues.ml *)
open Values
(* abstraction of sets of machine integers by parity *)
(* complete lattice *)
type t = BOT | ODD | EVEN | TOP
(*                TOP                                              *)
(*                /\                                               *)
(*               /  \                                              *)
(*              ODD EVEN                                           *)
(*               \Ê /                                              *)
(*		            \/                                               *)
(*                BOT                                              *)
(* \gamma(BOT)  = {_O_(a)}                                         *)
(* \gamma(ODD)  = { 2n+1\in[min_int,max_int] | n\in Z } U {_O_(a)} *)
(* \gamma(EVEN) = { 2n\in[min_int,max_int] | n\in Z } U {_O_(a)}   *)
(* \gamma(TOP)  = [min_int,max_int] U {_O_(a),_O_(i)}              *)
let bot () = BOT
(* bottom is emptyset? *)
let isbotempty () = false   (* \gamma(BOT) = {_O_(a)} <> \emptyset *)
(* uninitialization *)
let initerr () = TOP
(* supremum *)
let top () = TOP
(* least upper bound *)
let join v w =
  if (v = BOT) then w
  else if (w = BOT) then v
  else if (v = w) then w
  else TOP
(* greatest lower bound *)
let meet v w =
  if (v = TOP) then w
  else if (w = TOP) then v
  else if (v = w) then w
  else BOT
(* approximation ordering *)
let leq v w =
  if (v = BOT) then true
  else if( w = TOP) then true
  else v = w
(* equality *)
let eq u v = (u = v)
(* included in errors? *)
let in_errors u = (u = BOT)
(* printing *)
let print u = 
    match u with
    | BOT  -> print_string "_|_"
    | ODD  -> print_string "o"
    | EVEN -> print_string "e"
    | TOP  -> print_string "T"
(* forward abstract semantics of arithmetic expressions *)
(* f_NAT s = \alpha({(machine_int_of_string s)})        *)
let rec pry_of_intstring i s =
  let l = (String.length s) in
    if l = 0 then
      (if (i mod 2) = 0 then EVEN else ODD)
    else
      let v = (10 * i) + (int_of_string (String.sub s 0 1)) in
        if v < i then (* overflow *)
          BOT (* = \alpha({_O_(a)}) *)
        else
          pry_of_intstring v (String.sub s 1 (l-1))
let parity_of_intstring i = pry_of_intstring 0 i
let f_NAT i = parity_of_intstring i
(* f_RANDOM () = alpha([min_int, max_int]) *)
let f_RANDOM () = TOP
(* f_UMINUS a = alpha({Ê(machine_unary_minus x) | x \in gamma(a)} }) *)
let f_UMINUS u = u
(* f_UPLUS a = alpha(gamma(a)) *)
let f_UPLUS a = a
(* f_BINARITH a b = alpha({Ê(machine_binary_binarith i j)| }) |  *)
(*                              i in gamma(a) /\ j \in gamma(b)} *)
let nat_of_lat u =
  match u with
  | BOT  -> 0
  | ODD  -> 1
  | EVEN -> 2
  | TOP  -> 3
let select t u v = t.(nat_of_lat u). (nat_of_lat v)
let f_PLUS_table =
(*     +    BOT   ODD    EVEN   TOP  *)
(*BOT*)[|[| BOT ; BOT  ; BOT  ; BOT |];
(*ODD*)  [| BOT ; EVEN ; ODD  ; TOP |];
(*EVEN*) [| BOT ; ODD  ; EVEN ; TOP |];
(*TOP*)  [| BOT ; TOP  ; TOP  ; TOP |]|]
let f_PLUS  u v = select f_PLUS_table u v
let f_MINUS = f_PLUS
let f_TIMES_table =
(*     *    BOT   ODD    EVEN   TOP  *)
(*BOT*)[|[| BOT ; BOT ;  BOT  ; BOT |];
(*ODD*)  [| BOT ; ODD ;  EVEN ; TOP |];
(*EVEN*) [| BOT ; EVEN ; EVEN ; TOP |];
(*TOP*)  [| BOT ; TOP ;  TOP  ; TOP |]|]
let f_TIMES u v = select f_TIMES_table u v
let f_DIV_table =
(*     /    BOT   ODD   EVEN  TOP  *)
(*BOT*)[|[| BOT ; BOT ; BOT ; BOT |];
(*ODD*)  [| BOT ; TOP ; TOP ; TOP |];
(*EVEN*) [| BOT ; TOP ; TOP ; TOP |];
(*TOP*)  [| BOT ; TOP ; TOP ; TOP |]|]
let f_DIV   u v = select f_DIV_table u v
let f_MOD = f_DIV
(* forward abstract semantics of boolean expressions *)
(* Are there integer values in gamma(u) equal to values in gamma(v)? *)
let f_EQ u v = (u = TOP) || (v = TOP) || ((u = v) & (u != BOT))
(* Are there integer values in gamma(u) less than or equal to (<=) *)
(* integer values in gamma(v)?                                     *)
let f_LT u v = ((u != BOT) & (v != BOT))
(* widening *)
let widen v w = w
(* narrowing *)
let narrow v w = w
(* backward abstract semantics of arithmetic expressions *)
(* b_NAT s v = (machine_int_of_string s) in gamma(v) cap I? *)
exception Error_b_NAT of string
let b_NAT n p =
  match (String.get n (String.length n - 1)) with
  | '0'  -> leq EVEN p
  | '1'  -> leq ODD p
  | '2'  -> leq EVEN p
  | '3'  -> leq ODD p
  | '4'  -> leq EVEN p
  | '5'  -> leq ODD p
  | '6'  -> leq EVEN p
  | '7'  -> leq ODD p
  | '8'  -> leq EVEN p
  | '9'  -> leq ODD p
  | _    -> raise (Error_b_NAT "not a digit")
(* b_RANDOM p = gamma(p) cap I <> emptyset *)
let b_RANDOM p =
  match p with
  | BOT -> false
  | _   -> true
(* backward abstract semantics of arithmetic expressions       *)
(* b_NAT s v = (machine_int_of_string s) in gamma(v) cap       *)
(*                                         [min_int, max_int]? *)
let b_UMINUS q p = meet q p
let b_UPLUS q p = meet q p
(* b_BOP q1 q2 p = alpha2({<i1,i2>  in gamma2(<q1,q2>) |       *)
(*                         BOP(i1, i2) \in gamma(p) cap        *)
(*                                        [min_int, max_int]}) *)
exception Error_b_PLUS of string
let nat_of_lat' u =
  match u with
  | ODD  -> 0
  | EVEN -> 1
  | TOP  -> 2
  | _    -> raise (Error_b_PLUS "impossible selection")
let select' t u v = t.(nat_of_lat' u).(nat_of_lat' v)
let b_PLUS_ODD_table =
(*             ODD          EVEN        TOP      *)
(*ODD*)[|[| (BOT,BOT)  ; (ODD,EVEN) ; (ODD,EVEN) |];
(*EVEN*) [| (EVEN,ODD) ; (BOT,BOT)  ; (EVEN,ODD) |];
(*TOP*)  [| (EVEN,ODD) ; (ODD,EVEN) ; (TOP,TOP)  |]|]
let b_PLUS_EVEN_table =
(*             ODD         EVEN          TOP      *)
(*ODD*)[|[| (ODD,ODD) ; (BOT,BOT)   ; (ODD,ODD)   |];
(*EVEN*) [| (BOT,BOT) ; (EVEN,EVEN) ; (EVEN,EVEN) |];
(*TOP*)  [| (ODD,ODD) ; (EVEN,EVEN) ; (TOP,TOP)   |]|]
let b_PLUS q1 q2 p = 
  if (q1=BOT)||(q2=BOT)||(p=BOT) then
    (BOT,BOT)
  else if (p=TOP) then
    (q1,q2)
  else if p = ODD then  select' b_PLUS_ODD_table q1 q2
  else if p = EVEN then select' b_PLUS_EVEN_table q1 q2
  else raise (Error_b_PLUS "impossible case")
let b_MINUS = b_PLUS
let b_TIMES_ODD_table =
(*             ODD          EVEN        TOP      *)
(*ODD*)[|[| (ODD,ODD) ; (BOT,BOT) ; (ODD,ODD) |];
(*EVEN*) [| (BOT,BOT) ; (BOT,BOT) ; (BOT,BOT) |];
(*TOP*)  [| (ODD,ODD) ; (BOT,BOT) ; (ODD,ODD) |]|]
let b_TIMES_EVEN_table =
(*             ODD         EVEN          TOP      *)
(*ODD*)[|[| (BOT,BOT)  ; (ODD,EVEN)  ; (ODD,EVEN) |];
(*EVEN*) [| (EVEN,ODD) ; (EVEN,EVEN) ; (EVEN,TOP) |];
(*TOP*)  [| (EVEN,ODD) ; (TOP,EVEN)  ; (TOP,TOP)  |]|]
exception Error_b_TIMES of string
let b_TIMES q1 q2 p = 
  if (q1=BOT)||(q2=BOT)||(p=BOT) then
    (BOT,BOT)
  else if (p=TOP) then
    (q1,q2)
  else if p = ODD then  select' b_TIMES_ODD_table q1 q2
  else if p = EVEN then select' b_TIMES_EVEN_table q1 q2
  else raise (Error_b_TIMES "impossible case")
let b_DIV q1 q2 p = 
  if (q1=BOT)||(q2=BOT)||(p=BOT) then
(BOT,BOT)
  else
     (q1,q2)
let b_MOD = b_DIV
(* backward abstract interpretation of boolean expressions         *)
(* a_EQ p1 p2 = let p = p1 cap p2 cap [min_int, max_int] in <p, p> *)
let a_EQ p1 p2 = 
  let p = (meet p1 (meet p2 (f_RANDOM ()))) in
    (p,p)
(* a_LT p1 p2 = alpha({<i1, i2> | i1 in gamma(p1) cap [min_int,      *)
(*  max_int] /\ i2 in gamma(p1) cap [min_int, max_int] /\ i1 <= i2}) *)
let a_LT_table =
(*     <       BOT        ODD        EVEN          TOP     *)
(*BOT*)[|[| (BOT,BOT); (BOT,BOT) ; (BOT,BOT)  ; (BOT,BOT)  |];
(*ODD*)  [| (BOT,BOT); (ODD,ODD) ; (ODD,EVEN) ; (ODD,TOP)  |];
(*EVEN*) [| (BOT,BOT); (EVEN,ODD); (EVEN,EVEN); (EVEN,TOP) |];
(*TOP*)  [| (BOT,BOT); (TOP,ODD) ; (TOP,EVEN) ; (TOP,TOP)  |]|]
let a_LT   u v = select a_LT_table u v
