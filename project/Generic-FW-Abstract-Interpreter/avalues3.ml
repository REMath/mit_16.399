(* avalues.ml *)
open Values
(* abstraction of sets of machine integers by intervals *)
(* complete lattice *)
(*                  *)
(* ABSTRACT VALUES  *)
(*                  *)
type t = int * int
(* gamma (a,b)                                                    *)
(*   = [a,b] U {_O_(a), _O_(i)} when min_int <= a <= b <= max_int *)
(*   = {_O_(a), _O_(i)}         when a = max_int > min_int = b    *)
(* infimum: alpha({})                                             *)
let bottom = (max_int, min_int)
(* infimum: bot () = alpha({}) *)
let bot () = bottom
(* isbottom a = (a = bot) *)
let isbottom (x, y) = y < x
(* isbotempty () = gamma(bot ()) = {}?                            *)
let isbotempty () = false (* gamma([max_int, min_int]) =          *)
                          (* {_O_(a), _O_(i)} <> emptyset         *)
(* uninitialization: initerr () = alpha({_O_i}) *)
let initerr () = bottom
(* supremum: top () = alpha({_O_i, _O_a} U [min_int,max_int]) *)
let top () =  (min_int, max_int)
(* least upper bound join: p q = alpha(gamma(p) U gamma(q)) *)
let min x y = if (x <= y) then x else y
let max x y = if (x < y) then y else x
let join (v,w) (x,y) = ((min v x), (max w y))
(* greatest lower bound meet p q = alpha(gamma(p) cap gamma(q)) *)
let meet (v,w) (x,y) = ((max v x), (min w y))
(* approximation ordering: leq p q = gamma(p) subseteq gamma(q) *)
let leq (v,w) (x,y) = (isbottom (v, w)) || ((x <= v) && (w <= y))
(* equality: eq p q = gamma(p) = gamma(q) *)
let eq u v = (u = v)
(* errors = alpha({_O_i, _O_a}) *)
let errors = bottom
(* included in errors?: in_errors p = gamma(p) subseteq {_O_i, _O_a} *)
let in_errors (x, y) = isbottom (x, y)
(* printing *)
let print_int x =
   if x = min_int then print_string "min_int"
	else if x = - max_int then print_string "-max_int"
	else if x = max_int then print_string "max_int"
	else print_int x
let print (x, y) = if (isbottom (x, y)) then print_string "[]" else
   (print_string "["; print_int x; print_string ","; print_int y; 
    print_string "]")
(*                       *)
(* ABSTRACT TRANSFORMERS *)
(*                       *)
(* forward abstract semantics of arithmetic expressions *)
(* f_NAT s = alpha({(machine_int_of_string s)})        *)
let f_NAT s = 
   match (machine_int_of_string s) with
   | (ERROR_NAT INITIALIZATION) -> bottom
   | (ERROR_NAT ARITHMETIC) -> bottom
   | (NAT i) -> (i,i)
(* f_RANDOM () = alpha([min_int, max_int]) *)
let f_RANDOM () = (min_int, max_int)
(* f_UMINUS a = alpha({Ê(machine_unary_minus x) | x \in gamma(a)} }) *)
let f_UMINUS (x, y) = if (isbottom (x, y)) then bottom
   else if (x = min_int) then (-y, max_int)
	else (-y, -x)
(* f_UPLUS a = alpha(gamma(a)) *)
let f_UPLUS x = x
(* f_BINARITH a b = alpha({Ê(machine_binary_binarith i j) |      *)
(*                              i in gamma(a) /\ j \in gamma(b)} *)
let is_sum_lt_min_int x y =
	(* x + y < min_int *)
	if (x < 0) & (y < 0) then
		(x < min_int - y)
	else false
let is_sum_gt_max_int x y =
	(* x + y > max_int *)
		if (x > 0) && (y > 0) then
			(x > max_int - y)
		else false
let f_PLUS (a, b) (c, d) =
  if (isbottom (a, b)) || (isbottom (c, d)) then bottom
  else if (is_sum_gt_max_int a c) then bottom
  else if (is_sum_lt_min_int b d) then bottom
  else let lb = if (is_sum_lt_min_int a c) then min_int else a + c
       and ub = if (is_sum_gt_max_int b d) then max_int else b + d
       in (lb, ub)
let is_difference_lt_min_int x y =
	(* x - y < min_int *)
	if (x < 0) && (y > 0) then
		(x < min_int + y)
	else false
let is_difference_gt_max_int x y =
	(* x - y > max_int *)
	if (x > 0) && (y < 0) then
		(x > max_int + y)
	else false
let f_MINUS (a, b) (c, d) =
   if (isbottom (a, b)) || (isbottom (c, d)) then bottom
   else if (is_difference_gt_max_int a d) then bottom
	else if (is_difference_lt_min_int b c) then bottom
   else let lb = if (is_difference_lt_min_int a d) then min_int  
					  else a - d
        and ub = if (is_difference_gt_max_int b c) then max_int  
  			        else b - c
        in (lb, ub)
let sign x = if (x >= 0) then 1 else -1

exception Error_abs of string
let abs x = if (x >= 0) then x
				else if (x = min_int) then 
					raise (Error_abs "Incoherence: abs(min_int)")
				else (- x)
				
let times_int x y =
  if (x = 0) or (y = 0) then 0
  else if x = min_int then
	 (if y = 1 then min_int else if y < 0 then max_int else min_int)
  else if y = min_int then
	 (if x = 1 then min_int else if x < 0 then max_int else min_int)
  else if (sign x) * (sign y) > 0 then
	 (if (abs x) <= (max_int/(abs y)) then (x*y) else max_int)
  else if (abs x) = 1 then (x*y)
  else
	 (if (abs y) <= (min_int/(-(abs x))) then (x*y) else min_int)

let f_TIMES (x, y) (x', y') =
   if (isbottom (x, y)) || (isbottom (x', y')) then bottom
	else let a = times_int x x'
        and b = times_int x y' 
        and c = times_int y x' 
        and d = times_int y y' in
           ((min (min a b) (min c d)), (max (max a b) (max c d)))
			  
let rec f_DIV (x, y) (x', y') =
	if (isbottom (x, y)) || (isbottom (x', y')) || ((x' = 0) && (y' = 0)) 
	then bottom
	else if x' = 0 then f_DIV (x, y) (1, y')
	else if y' = 0 then f_DIV (x, y) (x', 1)
	else let a = x/x'
        and b = x/y' 
        and c = y/x' 
        and d = y/y' in
           ((min (min a b) (min c d)), (max (max a b) (max c d)))
let rec f_MOD (x, y) (x', y') =
	if (isbottom (x, y)) || (isbottom (x', y')) || (y < 0) || (y' < 1)
	then bottom
	else if x' < 0 then f_MOD (x, y) (0, y')
	else if y' <= 0 then f_MOD (x, y) (x', 1)
   else let a = x mod x'
	  and b = x mod y' 
	  and c = y mod x' 
	  and d = y mod y' in
		  ((min (min a b) (min c d)), (max (max a b) (max c d)))
(* forward abstract semantics of boolean expressions                  *)
(* Are there integer values in gamma(u) equal to values in gamma(v)? *)
(* f_LT p q = exists i in gamma(p) cap [min_int,max_int]:            *)
(*        exists j in gamma(q) cap [min_int,max_int]: machine_eq i j *)
let f_EQ (x, y) (x', y') = 
   if (isbottom (x, y)) || (isbottom (x', y')) then false
   else (min y y') <= (max x x')
(* Are there integer values in gamma(u) strictly less than (<)       *)
(* integer values in gamma(v)?                                       *)
(* f_LT p q = exists i in gamma(p) cap [min_int,max_int]:            *)
(*        exists j in gamma(q) cap [min_int,max_int]: machine_lt i j *)
let f_LT (x, y) (x', y') = 
   if (isbottom (x, y)) || (isbottom (x', y')) then false
   else (y' > x)
(* widening *)
(* let thresholds = [| |] (* only min_int and max_int *) *)
(* widening with  thresholds                             *)
let cmp i j = if i < j then -1 else if i = j then 0 else 1
let thresholds = let data = [| -1; 0; 1; |] in
                     (Array.sort cmp data; data) 
let widen (x, y) (x', y') =
  if (isbottom (x, y)) then (x', y')
  else if (isbottom (x', y')) then (x, y)
  else let lastindex = (Array.length thresholds) - 1 in
    let a = if x' >= x then x
       else let i = ref lastindex in
         (while (!i >= 0) & (x' < thresholds.(!i)) do 
			   i := !i - 1 
			 done;
          if (!i < 0) then min_int else thresholds.(!i))
   and b = if y' <= y then y
      else let j = ref 0 in
         (while (!j <= lastindex) & (y' > thresholds.(!j)) do 
			    j := !j + 1 
			 done;
          if (!j > lastindex) then max_int else thresholds.(!j))
   in a, b
(* narrowing *)
let narrow (x, y) (x', y') =
   if (isbottom (x, y)) || (isbottom (x', y')) then bottom
	else ((if (x = min_int) then x' else x), 
	                      (if (y = max_int) then y' else y))
(* backward abstract semantics of arithmetic expressions            *)
(* b_NAT s v = (machine_int_of_string s) in 
(¬                                 gamma(v) cap [min_int, max_int]? *)
let b_NAT s (a, b) = 
	match (machine_int_of_string s) with
	| (ERROR_NAT INITIALIZATION) -> false
	| (ERROR_NAT ARITHMETIC) -> false
	| (NAT i) -> (a <= i) && (i <= b)
(* b_RANDOM p = gamma(p) cap [min_int, max_int]  <> emptyset *)
let b_RANDOM p = not (isbottom p)
(* b_UOP q p = alpha({i in gamma(q) |                            *)
(*                  UOP(i) \in gamma(p) cap [min_int, max_int]}) *)
let b_UMINUS q (a, b) = meet q (-b, -a)
let b_UPLUS q p = meet q p
(* b_BOP q1 q2 p = alpha2({<i1,i2> in gamma2(<q1,q2>) |           *)
(*              BOP(i1, i2) \in gamma(p) cap [min_int, max_int]}) *)
let b_PLUS  (a, b) (c, d) (e, f) =
   if (in_errors (a, b)) || (in_errors (c, d)) then errors, errors 
   else if (in_errors (e, f)) then bottom, bottom
   else let lq1 = max a (if (is_difference_lt_min_int e d) 
                         then min_int  else (e - d))
        and uq1 = min b (if (is_difference_gt_max_int f c) 
		                 then max_int else (f - c))
		  and lq2 = max c (if (is_difference_lt_min_int e b) 
		                   then min_int else (e - b))
		  and uq2 = min d (if (is_difference_gt_max_int f a) 
		                   then max_int else (f - a)) 
		  in (if (lq1 <= uq1) then (lq1, uq1) else bottom), 
		     (if (lq2 <= uq2) then (lq2, uq2) else bottom)
let b_MINUS (a, b) (c, d) (e, f) =
   if (in_errors (a, b)) || (in_errors (c, d)) then errors, errors 
   else if (in_errors (e, f)) then bottom, bottom
   else b_PLUS  (a, b) (-d, -c) (e, f)
let b_TIMES (a, b) (c, d) (e, f) =
   if (in_errors (a, b)) || (in_errors (c, d)) then errors, errors 
   else if (in_errors (e, f)) then bottom, bottom
   else (a, b), (c, d)
let b_DIV (a, b) (c, d) (e, f) =
   if (in_errors (a, b)) || (in_errors (c, d)) then errors, errors 
   else if (in_errors (e, f)) then bottom, bottom
   else (a, b), (c, d)
let b_MOD (a, b) (c, d) (e, f) =
   if (in_errors (a, b)) || (in_errors (c, d)) then errors, errors 
   else if (in_errors (e, f)) then bottom, bottom
   else (a, b), (c, d)
(* backward abstract interpretation of boolean expressions *)
(* a_EQ p1 p2 = let p = p1 cap p2 cap [min_int, max_int]I in <p, p> *)
let a_EQ p1 p2 = let p = meet p1 p2 in (p, p)
(* a_LT p1 p2 = alpha2({<i1, i2> |                                *)
(*                      i1 in gamma(p1) cap [min_int, max_int] /\ *)
(*                      i2 in gamma(p1) cap [min_int, max_int] /\ *)  
(*                      i1 < i2})                                 *)
let a_LT (a, b) (c, d) =
if (isbottom (a, b)) || (isbottom (c, d)) || (a >= d) then 
  (bottom, bottom) else ((a, min b (d - 1)), (max (a + 1) c, d))
(* reduction with parity *)
(* information on parity = 0:EVEN, 1:ODD, 2:TOP *)
let parity (a, b) = if (a = b) then (a mod 2) else 2
(* reductions by parity *)
let reduce_even (a, b) = ((if ((a mod 2) = 0) then a 
                           else if a = max_int then a else (a+1)),
                          (if ((b mod 2) = 0) then b 
                           else if b = min_int then b else (b-1)))
let reduce_odd (a, b)  = ((if ((a mod 2) = 1) || ((a mod 2) = -1) 
                           then a 
                           else if a = max_int then a else (a+1)), 
                          (if ((b mod 2) = 1) || ((b mod 2) = -1) 
								   then b 
                           else if b = min_int then b else (b-1)))
(* reduction with initialization and simple sign *)
(* information on simple sign = -1:<0, 0:=0, 1:>0, 2:TOP *)
let sign (b1, b2) = (* b1 <= b2 *)
  if (b2 < 0) then -1
  else if (b1 = 0) & (b2 = 0) then 0
  else if (b1 > 0) then 1
  else 2
(* reductions by simple sign *)
let neg () = (min_int, -1)
let pos () = (1, max_int)
(* interval from bounds *)
let interval_from_bounds a b = if a <= b then (a, b) 
   else (max_int, min_int)
