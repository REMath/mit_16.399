(* avalues.ml *)
open Values
(* abstraction of sets of machine integers by initialization *)
(* and  simple sign *)
(* complete lattice *)
type t = BOT | NEG | ZERO | POS | INI | ERR | TOP
(* \gamma(BOT)  = {_O_(a)}                                   *)
(* \gamma(NEG)  = [min_int,-1] U {_O_(a)}                    *)
(* \gamma(POS)  = [1,max_int] U {_O_(a)}                     *)
(* \gamma(ZERO) = {0, _O_(a)}                                *)
(* \gamma(INI)  = [min_int,max_int] U {_O_(a)}               *)
(* \gamma(ERR)  = {_O_(i),_O_(a)}                            *)
(* \gamma(TOP)  = [min_int,max_int] U {_O_(a),_O_(i)}        *)
(* infimum *)
let bot () = BOT 
(* bottom is emptyset? *)
let isbotempty () = false (* \gamma(BOT) = {_O_(a)} <> \emptyset *)
(* uninitialization *)
let initerr () = ERR
(* supremum *)
let top () = TOP
(* least upper bound *)
let nat_of_lat u =
		match u with
		| BOT  -> 0
		| NEG  -> 1
		| ZERO -> 2
		| POS  -> 3
		| INI  -> 4
		| ERR  -> 5
		| TOP  -> 6
let select t u v = t.(nat_of_lat u).(nat_of_lat v)
let join_table =
(*          BOT    NEG   ZERO   POS   INI   ERR   TOP  *)
(*BOT*)[|[| BOT ;  NEG ; ZERO ; POS ; INI ; ERR ; TOP |];
(*NEG*)  [| NEG ;  NEG ; INI  ; INI ; INI ; TOP ; TOP |];
(*ZERO*) [| ZERO ; INI ; ZERO ; INI ; INI ; TOP ; TOP |];
(*POS*)  [| POS ;  INI ; INI  ; POS ; INI ; TOP ; TOP |];
(*INI*)  [| INI ;  INI ; INI  ; INI ; INI ; TOP ; TOP |];
(*ERR*)  [| ERR ;  TOP ; TOP  ; TOP ; TOP ; ERR ; TOP |];
(*TOP*)  [| TOP ;  TOP ; TOP  ; TOP ; TOP ; TOP ; TOP |]|]
let join u v = select join_table u v
(* greatest lower bound *)
let meet_table =
(*          BOT   NEG   ZERO   POS   INI   ERR   TOP   *)
(*BOT*)[|[| BOT ; BOT ; BOT  ; BOT ; BOT ;  BOT ; BOT  |];
(*NEG*)  [| BOT ; NEG ; BOT  ; BOT ; NEG ;  BOT ; NEG  |];
(*ZERO*) [| BOT ; BOT ; ZERO ; BOT ; ZERO ; BOT ; ZERO |];
(*POS*)  [| BOT ; BOT ; BOT  ; POS ; POS ;  BOT ; POS  |];
(*INI*)  [| BOT ; NEG ; ZERO ; POS ; INI ;  BOT ; INI  |];
(*ERR*)  [| BOT ; BOT ; BOT  ; BOT ; BOT ;  ERR ; ERR  |];
(*TOP*)  [| BOT ; NEG ; ZERO ; POS ; INI ;  ERR ; TOP  |]|]
let meet u v = select meet_table u v 
(* approximation ordering *)
let leq_table =
(*          BOT     NEG     ZERO    POS     INI     ERR     TOP  *)
(*BOT*)[|[| true ;  true ;  true ;  true ;  true ;  true ;  true |];
(*NEG*)  [| false ; true ;  false ; false ; true ;  false ; true |];
(*ZERO*) [| false ; false ; true ;  false ; true ;  false ; true |];
(*POS*)  [| false ; false ; false ; true ;  true ;  false ; true |];
(*INI*)  [| false ; false ; false ; false ; true ;  false ; true |];
(*ERR*)  [| false ; false ; false ; false ; false ; true ;  true |];
(*TOP*)  [| false ; false ; false ; false ; false ; false ; true |]|]
let leq u v = select leq_table u v 
(* equality *)
let eq u v = (u = v)
(* included in errors? *)
let in_errors v = (leq v ERR)
(* printing *)
let print u = 
  match u with
  | BOT  -> (print_string "BOT")
  | NEG  -> (print_string "NEG")
  | ZERO -> (print_string "ZERO")
  | POS  -> (print_string "POS")
  | INI  -> (print_string "INI")
  | ERR  -> (print_string "ERR")
  | TOP  -> (print_string "TOP")
(* forward abstract semantics of arithmetic expressions *)
let f_NAT s = 
   match (machine_int_of_string s) with
	| (ERROR_NAT INITIALIZATION) -> ERR
	| (ERROR_NAT ARITHMETIC) -> BOT
	| (NAT i) -> if i = 0 then ZERO else if i > 0 then POS else NEG
let f_RANDOM () = INI
let f_UMINUS a = 
   match a with
   | BOT  -> BOT
   | NEG  -> POS
   | ZERO -> ZERO
   | POS  -> NEG
   | INI  -> INI
   | ERR  -> ERR
   | TOP  -> TOP
let f_UPLUS a = a
let f_PLUS_table =
(*     +   BOT   NEG   ZERO   POS   INI    ERR   TOP  *)
(*BOT*)[|[| BOT ; BOT ; BOT  ; BOT ; BOT ; BOT ; BOT |];
(*NEG*)  [| BOT ; NEG ; NEG  ; INI ; INI ; ERR ; TOP |];
(*ZERO*) [| BOT ; NEG ; ZERO ; POS ; INI ; ERR ; TOP |];
(*POS*)  [| BOT ; INI ; POS  ; POS ; INI ; ERR ; TOP |];
(*INI*)  [| BOT ; INI ; INI  ; INI ; INI ; ERR ; TOP |];
(*ERR*)  [| ERR ; ERR ; ERR  ; ERR ; ERR ; ERR ; ERR |];
(*TOP*)  [| ERR ; TOP ; TOP  ; TOP ; TOP ; ERR ; TOP |]|]
let f_PLUS  u v = select f_PLUS_table u v
let f_MINUS_table =
(*     -    BOT   NEG   ZERO   POS   INI    ERR   TOP  *)
(*BOT*)[|[| BOT ; BOT ; BOT  ; BOT ; BOT ; BOT ; BOT |];
(*NEG*)  [| BOT ; INI ; NEG  ; NEG ; INI ; ERR ; TOP |];
(*ZERO*) [| BOT ; POS ; ZERO ; NEG ; INI ; ERR ; TOP |];
(*POS*)  [| BOT ; POS ; POS  ; INI ; INI ; ERR ; TOP |];
(*INI*)  [| BOT ; INI ; INI  ; INI ; INI ; ERR ; TOP |];
(*ERR*)  [| ERR ; ERR ; ERR  ; ERR ; ERR ; ERR ; ERR |];
(*TOP*)  [| ERR ; TOP ; TOP  ; TOP ; TOP ; ERR ; TOP |]|]
let f_MINUS u v = select f_MINUS_table u v
let f_TIMES_table =
(*     *    BOT   NEG    ZERO   POS    INI    ERR   TOP  *)
(*BOT*)[|[| BOT ; BOT ;  BOT  ; BOT ;  BOT ;  BOT ; BOT |];
(*NEG*)  [| BOT ; POS ;  ZERO ; NEG ;  INI ;  ERR ; TOP |];
(*ZERO*) [| BOT ; ZERO ; ZERO ; ZERO ; ZERO ; ERR ; TOP |];
(*POS*)  [| BOT ; NEG ;  ZERO ; POS ;  INI ;  ERR ; TOP |];
(*INI*)  [| BOT ; INI ;  ZERO ; INI ;  INI ;  ERR ; TOP |];
(*ERR*)  [| ERR ; ERR ;  ERR  ; ERR ;  ERR ;  ERR ; ERR |];
(*TOP*)  [| ERR ; TOP ;  TOP  ; TOP ;  TOP ;  ERR ; TOP |]|]
let f_TIMES u v = select f_TIMES_table u v
let f_DIV_table =
(*     /    BOT   NEG   ZERO  POS   INI    ERR   TOP  *)
(*BOT*)[|[| BOT ; BOT ; BOT ; BOT ;  BOT ; BOT ; BOT |];
(*NEG*)  [| BOT ; BOT ; BOT ; BOT ;  BOT ; BOT ; BOT |];
(*ZERO*) [| BOT ; BOT ; BOT ; ZERO ; POS ; ERR ; TOP |];
(*POS*)  [| BOT ; BOT ; BOT ; INI ;  INI ; ERR ; TOP |];
(*INI*)  [| BOT ; BOT ; BOT ; INI ;  INI ; ERR ; TOP |];
(*ERR*)  [| ERR ; ERR ; ERR ; ERR ;  ERR ; ERR ; ERR |];
(*TOP*)  [| ERR ; ERR ; ERR ; TOP ;  TOP ; ERR ; TOP |]|]
let f_DIV   u v = select f_DIV_table u v
let f_MOD_table =
(*    mod   BOT   NEG   ZERO  POS    INI    ERR   TOP  *)
(*BOT*)[|[| BOT ; BOT ; BOT ; BOT  ; BOT  ; BOT ; BOT |];
(*NEG*)  [| BOT ; BOT ; BOT ; BOT  ; BOT  ; BOT ; BOT |];
(*ZERO*) [| BOT ; BOT ; BOT ; ZERO ; ZERO ; ERR ; TOP |];
(*POS*)  [| BOT ; BOT ; BOT ; INI  ; INI  ; ERR ; TOP |];
(*INI*)  [| BOT ; BOT ; BOT ; INI  ; INI  ; ERR ; TOP |];
(*ERR*)  [| ERR ; ERR ; ERR ; ERR  ; ERR  ; ERR ; ERR |];
(*TOP*)  [| ERR ; ERR ; ERR ; TOP  ; TOP  ; ERR ; TOP |]|]
let f_MOD   u v = select f_MOD_table u v
(* forward abstract semantics of boolean expressions *)
let f_EQ_table =
(*    mod   BOT     NEG     ZERO    POS     INI     ERR     TOP   *)
(*BOT*)[|[| false ; false ; false ; false ; false ; false ; false |];
(*NEG*)  [| false ; true  ; false ; false ; true  ; false ; true  |];
(*ZERO*) [| false ; false ; true  ; false ; true  ; false ; true  |];
(*POS*)  [| false ; false ; false ; true  ; true  ; false ; true  |];
(*INI*)  [| false ; true  ; true  ; true  ; true  ; false ; true  |];
(*ERR*)  [| false ; false ; false ; false ; false ; false ; false |];
(*TOP*)  [| false ; true  ; true  ; true  ; true  ; false ; true  |]|]
(* Are there integer values in gamma(u) equal to values in gamma(v)? *)
let f_EQ u v = select f_EQ_table u v
let f_LT_table =
(*    mod   BOT     NEG     ZERO    POS     INI     ERR     TOP   *)
(*BOT*)[|[| false ; false ; false ; false ; false ; false ; false |];
(*NEG*)  [| false ; true  ; true  ; true  ; true  ; false ; true  |];
(*ZERO*) [| false ; false ; true  ; true  ; true  ; false ; true  |];
(*POS*)  [| false ; false ; false ; true  ; true  ; false ; true  |];
(*INI*)  [| false ; true  ; true  ; true  ; true  ; false ; true  |];
(*ERR*)  [| false ; false ; false ; false ; false ; false ; false |];
(*TOP*)  [| false ; true  ; true  ; true  ; true  ; false ; true  |]|]
(* Are there integer values in gamma(u) less than or equal to (<=) *)
(* integer values in gamma(v)?                                     *)
let f_LT u v = select f_LT_table u v
(* backward abstract interpretation of arithmetic expressions *)
exception Error_f_NAT of string
let remove_zeros i =
  let l = (String.length i) in
  if l = 0 then raise (Error_f_NAT "empty integer")
  else if l = 1 then i
  else if (String.get i 0) = '0' then String.sub i 1  (l - 1)
  else i
let b_NAT i p =
  let i' = (remove_zeros i) in
  if i' = "0" then
    match p with
    | BOT  -> false
    | NEG  -> false
    | ZERO -> true
    | POS  -> false
    | INI  -> true
    | ERR  -> false
    | TOP  -> true
  else
    match p with
    | BOT  -> false
    | NEG  -> false
    | ZERO -> false
    | POS  -> true
    | INI  -> true
    | ERR  -> false
    | TOP  -> true
let b_RANDOM p =
  match p with
  | BOT -> false
  | ERR -> false
  | _   -> true
let b_UMINUS q p =
  match p with
  | BOT  -> BOT
  | NEG  -> meet q POS
  | ZERO -> meet q ZERO
  | POS  -> meet q NEG
  | INI  -> meet q INI
  | ERR  -> BOT
  | TOP  -> meet q INI
let b_UPLUS q p =
  match p with
  | BOT -> BOT
  | ERR -> BOT
  | TOP -> meet q INI
  | _   -> meet q p
exception Error_b_PLUS of string
let nat_of_lat' u =
  match u with
  | NEG  -> 0
  | ZERO -> 1
  | POS  -> 2
  | INI  -> 3
  | TOP  -> 4
  | _    -> raise (Error_b_PLUS "impossible selection")
let select' t u v = t.(nat_of_lat' u).(nat_of_lat' v)
let b_PLUS_NEG_table =
(*             NEG         ZERO         POS         INI          TOP      *)
(*NEG*)[|[| (NEG,NEG) ;  (NEG,ZERO) ; (NEG,POS) ; (NEG,INI) ;  (NEG,INI)  |];
(*ZERO*) [| (ZERO,NEG) ; (BOT,BOT) ;  (BOT,BOT) ; (ZERO,NEG) ; (ZERO,NEG) |];
(*POS*)  [| (POS,NEG) ;  (BOT,BOT) ;  (BOT,BOT) ; (POS,NEG) ;  (POS,NEG)  |];
(*INI*)  [| (INI,NEG) ;  (NEG,ZERO) ; (NEG,POS) ; (INI,INI) ;  (INI,INI)  |];
(*TOP*)  [| (INI,NEG) ;  (NEG,ZERO) ; (NEG,POS) ; (INI,INI) ;  (INI,INI)  |]|]
let b_PLUS_ZERO_table =
(*NEG*)[|[| (BOT,BOT) ; (BOT,BOT) ;   (NEG,POS) ; (NEG,POS) ;   (NEG,POS)   |];
(*ZERO*) [| (BOT,BOT) ; (ZERO,ZERO) ; (BOT,BOT) ; (ZERO,ZERO) ; (ZERO,ZERO) |];
(*POS*)  [| (POS,NEG) ; (BOT,BOT) ;   (BOT,BOT) ; (POS,NEG) ;   (POS,NEG)   |];
(*INI*)  [| (POS,NEG) ; (ZERO,ZERO) ; (NEG,POS) ; (INI,INI) ;   (INI,INI)   |];
(*TOP*)  [| (POS,NEG) ; (ZERO,ZERO) ; (NEG,POS) ; (INI,INI) ;   (INI,INI)   |]|]
let b_PLUS_POS_table =
(*NEG*)[|[| (BOT,BOT) ; (BOT,BOT) ;  (NEG,POS) ;  (NEG,POS) ;  (NEG,POS)  |];
(*ZERO*) [| (BOT,BOT) ; (BOT,BOT) ;  (ZERO,POS) ; (ZERO,POS) ; (ZERO,POS) |];
(*POS*)  [| (POS,NEG) ; (POS,ZERO) ; (POS,INI) ;  (POS,INI) ;  (POS,INI)  |];
(*INI*)  [| (POS,NEG) ; (POS,ZERO) ; (INI,POS) ;  (INI,INI) ;  (INI,INI)  |];
(*TOP*)  [| (POS,NEG) ; (POS,ZERO) ; (INI,POS) ;  (INI,INI) ;  (INI,INI)  |]|]
let b_PLUS q1 q2 p = 
  if (q1=BOT)||(q1=ERR)||(q2=BOT)||(q2=ERR)||(p=BOT)||(p=ERR) then
    (BOT,BOT)
  else if (p=INI)||(p=TOP) then
    ((meet q1 INI),(meet q2 INI))
  else if p = NEG then  select' b_PLUS_NEG_table q1 q2
  else if p = ZERO then select' b_PLUS_ZERO_table q1 q2
  else if p = POS then  select' b_PLUS_POS_table q1 q2
  else raise (Error_b_PLUS "impossible case")
let b_MINUS q1 q2 p = 
  let r1,r2 = b_PLUS q1 (f_UMINUS q2) p in
    r1,(f_UMINUS  r2)
exception Error_b_TIMES of string
let b_TIMES_NEG_table =
(*             NEG         ZERO         POS         INI          TOP      *)
(*NEG*)[|[| (BOT,BOT) ; (BOT,BOT) ; (BOT,BOT) ; (NEG,INI) ; (NEG,INI) |];
(*ZERO*) [| (BOT,BOT) ; (BOT,BOT) ; (BOT,BOT) ; (BOT,BOT) ; (BOT,BOT) |];
(*POS*)  [| (POS,NEG) ; (BOT,BOT) ; (BOT,BOT) ; (POS,NEG) ; (POS,NEG) |];
(*INI*)  [| (INI,NEG) ; (BOT,BOT) ; (NEG,POS) ; (INI,INI) ; (INI,INI) |];
(*TOP*)  [| (INI,NEG) ; (BOT,BOT) ; (NEG,POS) ; (INI,INI) ; (INI,INI) |]|]
let b_TIMES_ZERO_table =
(*NEG*)[|[| (BOT,BOT) ;  (NEG,ZERO) ;  (BOT,BOT) ;  (NEG,ZERO) ; (NEG,ZERO) |];
(*ZERO*) [| (ZERO,NEG) ; (ZERO,ZERO) ; (ZERO,POS) ; (ZERO,INI) ; (ZERO,INI) |];
(*POS*)  [| (BOT,BOT) ;  (POS,ZERO) ;  (BOT,BOT) ;  (POS,ZERO) ; (POS,ZERO) |];
(*INI*)  [| (ZERO,NEG) ; (INI,ZERO) ;  (ZERO,POS) ; (INI,INI) ;  (INI,INI)  |];
(*TOP*)  [| (ZERO,NEG) ; (INI,ZERO) ;  (ZERO,POS) ; (INI,INI) ;  (INI,INI)  |]|]
let b_TIMES_POS_table =
(*NEG*)[|[| (BOT,BOT) ; (BOT,BOT) ; (BOT,BOT) ; (NEG,NEG) ; (NEG,NEG) |];
(*ZERO*) [| (BOT,BOT) ; (BOT,BOT) ; (BOT,BOT) ; (BOT,BOT) ; (BOT,BOT) |];
(*POS*)  [| (POS,NEG) ; (BOT,BOT) ; (POS,POS) ; (POS,POS) ; (POS,POS) |];
(*INI*)  [| (POS,NEG) ; (BOT,BOT) ; (POS,POS) ; (INI,INI) ; (INI,INI) |];
(*TOP*)  [| (POS,NEG) ; (BOT,BOT) ; (POS,POS) ; (INI,INI) ; (INI,INI) |]|]
let b_TIMES q1 q2 p = 
  if (q1=BOT)||(q1=ERR)||(q2=BOT)||(q2=ERR)||(p=BOT)||(p=ERR) then
    (BOT,BOT)
  else if (p=INI)||(p=TOP) then
    ((meet q1 INI),(meet q2 INI))
  else if p = NEG then  select' b_TIMES_NEG_table q1 q2
  else if p = ZERO then select' b_TIMES_ZERO_table q1 q2
  else if p = POS then  select' b_TIMES_POS_table q1 q2
  else raise (Error_b_TIMES "impossible case")
let smash x y = if (x=BOT)||(y=BOT) then (BOT,BOT) else (x,y)
let b_DIV q1 q2 p = 
  if (q1=BOT)||(q1=NEG)||(q1=ERR)||
     (q2=BOT)||(q2=NEG)||(q2=ZERO)||(q2=ERR)||
(p=BOT)||(p=NEG)||(p=ERR) then
(BOT,BOT)
  else if p = POS then
     (smash (meet q1 POS) (meet q2 POS))
  else
     (smash (meet q1 INI) (meet q2 POS))
let b_MOD = b_DIV
(* boolean expressions *)
let a_EQ p1 p2 = 
  let p = (meet p1 (meet p2 (f_RANDOM ()))) in
    (p,p)
let a_LT_table =[|
(*     <      BOT       NEG       ZERO       POS        INI        ERR       TOP    *)
(*BOT*) [|(BOT,BOT);(BOT,BOT);(BOT,BOT) ;(BOT,BOT) ;(BOT,BOT) ;(BOT,BOT);(BOT,BOT) |];
(*NEG*) [|(BOT,BOT);(NEG,NEG);(NEG,ZERO);(NEG,POS) ;(NEG,INI) ;(BOT,BOT);(NEG,INI) |];
(*ZERO*)[|(BOT,BOT);(BOT,BOT);(BOT,BOT) ;(ZERO,POS);(ZERO,POS);(BOT,BOT);(ZERO,POS)|];
(*POS*) [|(BOT,BOT);(BOT,BOT);(BOT,BOT) ;(POS,POS) ;(POS,POS) ;(BOT,BOT);(POS,POS) |];
(*INI*) [|(BOT,BOT);(NEG,NEG);(NEG,ZERO);(INI,POS) ;(INI,INI) ;(BOT,BOT);(INI,INI) |];
(*ERR*) [|(BOT,BOT);(BOT,BOT);(BOT,BOT) ;(BOT,BOT) ;(BOT,BOT) ;(BOT,BOT);(BOT,BOT) |];
(*TOP*) [|(BOT,BOT);(NEG,NEG);(NEG,ZERO);(INI,POS) ;(INI,INI) ;(BOT,BOT);(INI,INI) |]
|]
let a_LT   u v = select a_LT_table u v
