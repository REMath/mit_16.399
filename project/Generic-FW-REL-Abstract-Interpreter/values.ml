(* values.ml *)
type error_type = INITIALIZATION | ARITHMETIC
type machine_int = ERROR_NAT of error_type | NAT of int
type machine_bool = ERROR_BOOL of error_type | BOOLEAN of bool
						 
let machine_unary_random () = 
   let r = (Random.int max_int) in
	  if Random.bool () then (NAT r) else (NAT ((- r) - 1))

let machine_unary_plus a = a

let machine_unary_minus a = match a with
|  ERROR_NAT e -> (ERROR_NAT e)
|  NAT i   -> if i = min_int then
                 (ERROR_NAT ARITHMETIC)
				  else
				     (NAT (- i))

let add_int x y =
  if (x >= 0) & (y >= 0) then
     (if x <= (max_int - y) then (NAT (x+y)) else (ERROR_NAT ARITHMETIC))
  else if (x <= 0) & (y <= 0) then
     (if (min_int - x) <= y then (NAT (x+y)) else (ERROR_NAT ARITHMETIC))
  else 
    (NAT (x+y))

let machine_binary_plus a b = match a with
|	ERROR_NAT e -> (ERROR_NAT e)
|	NAT a'  -> match b with
				  | ERROR_NAT e' -> (ERROR_NAT e')
				  | NAT b' -> (add_int a' b')

let sub_int x y =
   if (x >= 0) & (y <= 0) then
      (if x <= (max_int + y) then (NAT (x-y)) else (ERROR_NAT ARITHMETIC))
   else if (x <= 0) & (y >= 0) then
      (if x < (min_int + y) then (ERROR_NAT ARITHMETIC) else (NAT (x-y)))
   else 
      (NAT (x-y))

let machine_binary_minus a b = match a with
|	ERROR_NAT e -> (ERROR_NAT e)
|	NAT a'  -> match b with
		        | ERROR_NAT e' -> (ERROR_NAT e')
              | NAT b' -> (sub_int a' b')

let sign x = if (x >= 0) then 1 else -1

exception Error_abs of string
let abs x = if (x >= 0) then x
            else if (x = min_int) then 
			      raise (Error_abs "Incoherence: abs(min_int)")
				else (- x)
				
let times_int x y =
  if (x = 0) or (y = 0) then (NAT 0)
  else if x = min_int then
    (if y = 1 then (NAT min_int) else (ERROR_NAT ARITHMETIC))
  else if y = min_int then
    (if x = 1 then (NAT min_int) else (ERROR_NAT ARITHMETIC))
  else if (sign x) * (sign y) > 0 then
    (if (abs x) <= (max_int/(abs y)) then (NAT (x*y)) else (ERROR_NAT ARITHMETIC))
  else if (abs x) = 1 then (NAT (x*y)) 
  else
    (if (abs y) <= (min_int/(-(abs x))) then (NAT (x*y)) else (ERROR_NAT ARITHMETIC))

let machine_binary_times a b = match a with
|	ERROR_NAT e -> (ERROR_NAT e)
|	NAT a'  -> match b with
              | ERROR_NAT e' -> (ERROR_NAT e')
              | NAT b' -> (times_int a' b')

let div_int i j =
  if (j = 0) then (ERROR_NAT ARITHMETIC) else (NAT (i / j))

let machine_binary_div a b = match a with
|	ERROR_NAT e -> (ERROR_NAT e)
|	NAT a'  -> match b with
              | ERROR_NAT e' -> (ERROR_NAT e')
              | NAT b' -> (div_int a' b')

let mod_int i j =
  if (i<0) or (j<=0) then (ERROR_NAT ARITHMETIC) else (NAT (i mod j))

let machine_binary_mod a b = match a with
|	ERROR_NAT e -> (ERROR_NAT e)
|	NAT a'  -> match b with
  | ERROR_NAT e' -> (ERROR_NAT e')
  | NAT b' -> (mod_int a'  b')

exception Incorrect_Digit of char
let machine_int_of_char c =
	 if ('0' <= c) && (c <= '9') then
		 NAT( (Char.code c) - (Char.code '0'))
	 else
		 raise (Incorrect_Digit c)
		 
exception Incorrect_Nat of string
let rec int_of_intstring i s =
  let l = (String.length s) in
	 if l = 0 then
		(NAT i)
	 else
		let v = (10 * i) + (int_of_string (String.sub s 0 1)) in
		  if v<i then (* overflow *)
		    (ERROR_NAT ARITHMETIC)
		  else
			 int_of_intstring v (String.sub s 1 (l-1))
let machine_int_of_string s = 
  int_of_intstring 0 s

let machine_eq a b = match a with
|	ERROR_NAT e -> (ERROR_BOOL e)
|	NAT a'  -> match b with
  | ERROR_NAT e' -> (ERROR_BOOL e')
  | NAT b' -> (BOOLEAN (a' = b'))
		 
let machine_lt a b = match a with
|	ERROR_NAT e -> (ERROR_BOOL e)
|	NAT a'  -> match b with
  | ERROR_NAT e' -> (ERROR_BOOL e')
  | NAT b' -> (BOOLEAN (a' < b'))

let machine_and a b = match a with
|	ERROR_BOOL e -> (ERROR_BOOL e)
|	BOOLEAN a'  -> match b with
  | ERROR_BOOL e' -> (ERROR_BOOL e')
  | BOOLEAN b' -> (BOOLEAN (a' & b'))

let machine_or a b = match a with
|	ERROR_BOOL e -> (ERROR_BOOL e)
|	BOOLEAN a'  -> match b with
  | ERROR_BOOL e' -> (ERROR_BOOL e')
  | BOOLEAN b' -> (BOOLEAN (a' or b'))

let print_machine_int a = match a with
|	ERROR_NAT INITIALIZATION -> print_string "_O_(i)"
|	ERROR_NAT ARITHMETIC -> print_string "_O_(a)"
|	NAT a'  -> print_int a'
  