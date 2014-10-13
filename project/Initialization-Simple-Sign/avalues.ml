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
