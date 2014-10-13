(* avalues.ml *)
open Values
(* abstraction of sets of machine integers by errors *)
(* complete lattice *)
type t = NER | AER | IER | ERR
(* gamma(NER) = [min_int,max_int]                    *)
(* gamma(AER) = [min_int,max_int] U {_O_(a)}         *)
(* gamma(IER) = [min_int,max_int] U {_O_(i)}         *)
(* gamma(ERR) = [min_int,max_int] U {_O_(a), _O_(i)} *)
(* infimum *)
let bot () = NER
(* bottom is emptyset? *)
let isbotempty () = false
(* uninitialization *)
let initerr () = IER
(* supremum *)
let top () =  ERR
(* least upper bound *)
let nat_of_lat u =
      match u with
      | NER -> 0
      | AER -> 1
      | IER -> 2
      | ERR -> 3
let select t u v = t.(nat_of_lat u).(nat_of_lat v)
let join_table =
(*          NER   AER   IER   ERR    *)
(*NER*)[|[| NER ; AER ; IER ; ERR ; |];
(*AER*)  [| AER ; AER ; ERR ; ERR ; |];
(*IER*)  [| IER ; ERR ; IER ; ERR ; |];
(*ERR*)  [| ERR ; ERR ; ERR ; ERR ; |]|]
let join u v = select join_table u v
(* greatest lower bound *)
let meet_table =
(*          NER   AER   IER   ERR    *)
(*NER*)[|[| NER ; NER ; NER ; NER ; |];
(*AER*)  [| NER ; AER ; NER ; AER ; |];
(*IER*)  [| NER ; NER ; IER ; IER ; |];
(*ERR*)  [| NER ; AER ; IER ; ERR ; |]|]
let  meet u v = select join_table u v
(* approximation ordering *)
let leq_table =
(*          NER     AER     IER     ERR    *)
(*NER*)[|[| true ;  true ;  true ;  true ; |];
(*AER*)  [| false ; true ;  false ; true ; |];
(*IER*)  [| false ; false ; true ;  true ; |];
(*ERR*)  [| false ; false ; false ; true ; |]|]
let leq u v = select leq_table u v 
(* equality *)
let eq u v = (u = v)
(* included in errors? *)
let in_errors v = (leq v ERR)
(* printing *)
let print u = match u with
| NER -> print_string "{}" 
| AER -> print_string "{_O_a}"  
| IER -> print_string "{_O_i}"  
| ERR -> print_string "{_O_a,_O_i}" 
(* forward abstract semantics of arithmetic expressions *)
(* f_NAT s = \alpha({(machine_int_of_string s)})        *)
let f_NAT s = 
   match (machine_int_of_string s) with
   | (ERROR_NAT INITIALIZATION) -> IER
   | (ERROR_NAT ARITHMETIC) -> AER
   | (NAT i) -> NER
(* f_RANDOM () = alpha([min_int, max_int]) *)
let f_RANDOM () = NER
(* f_UMINUS a = alpha({Ê(machine_unary_minus x) | x \in gamma(a)} }) *)
let f_UMINUS a = 
   match a with
   | NER -> AER (* a can be min_int *)
   | AER -> AER
   | IER -> IER
   | ERR -> ERR
(* f_UPLUS a = alpha(gamma(a)) *)
let f_UPLUS a = a
(* f_BINARITH a b = alpha({Ê(machine_binary_binarith i j)| }) |  *)
(*                              i in gamma(a) /\ j \in gamma(b)} *)
let f_BINARITH a b =
   match a with
   | NER -> (match b with
             | NER -> AER
             | AER -> AER
             | IER -> IER
             | ERR -> ERR)
   | AER -> AER
   | IER -> IER
   | ERR -> ERR
let f_PLUS  = f_BINARITH
let f_MINUS = f_BINARITH
let f_TIMES = f_BINARITH
let f_DIV   = f_BINARITH
let f_MOD   = f_BINARITH
(* forward abstract semantics of boolean expressions *)
(* Are there integer values in gamma(u) equal to values in gamma(v)? *)
let f_EQ u v = true
(* Are there integer values in gamma(u) less than or equal to (<=) *)
(* integer values in gamma(v)?                                     *)
let f_LT u v = true
(* widening *)
let widen v w = w
(* narrowing *)
let narrow v w = w
(* backward abstract semantics of arithmetic expressions *)
(* b_NAT s v = (machine_int_of_string s) in gamma(v) cap I? *)
let b_NAT s p = 
	match (machine_int_of_string s) with
	| (ERROR_NAT INITIALIZATION) -> false
	| (ERROR_NAT ARITHMETIC) -> false
	| (NAT i) -> true
(* b_RANDOM p = gamma(p) cap I <> emptyset *)
let b_RANDOM p = true
(* b_UOP q p = alpha({i in gamma(q) | UOP(i) \in gamma(p) cap   *)
(*                                         [min_int, max_int]}) *)
let b_UMINUS q p = NER
let b_UPLUS q p = NER
(* b_BOP q1 q2 p = alpha2({<i1,i2>  in gamma2(<q1,q2>) |        *)
(*            BOP(i1, i2) \in gamma(p) cap [min_int, max_int]}) *)
let b_PLUS q1 q2 p = NER, NER
let b_MINUS q1 q2 p = NER, NER
let b_TIMES q1 q2 p = NER, NER
let b_DIV q1 q2 p = NER, NER
let b_MOD q1 q2 p = NER, NER
(* backward abstract interpretation of boolean expressions         *)
(* a_EQ p1 p2 = let p = p1 cap p2 cap [min_int, max_int] in <p, p> *)
let a_EQ p1 p2 = NER, NER
(* a_LT p1 p2 = alpha({<i1, i2> |                                  *)
(*            i1 in gamma(p1) cap [min_int, max_int] /\            *)
(*            i2 in gamma(p1) cap [min_int, max_int] /\ i1 <= i2}) *)
let a_LT p1 p2 = NER, NER