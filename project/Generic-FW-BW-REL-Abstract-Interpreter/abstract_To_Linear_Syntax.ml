(* abstract_To_Linear_Syntax.ml *)
open Abstract_Syntax   
open Linear_Syntax
open Values
open Variables
(* Linearization of arithmetic operations *)
exception Not_constant
exception Not_linear
exception Abstract_To_Linear_Syntax_error
let rec linearize_aexp a = 
 let n = (number_of_variables ()) in
  try
   match a with
   | (Abstract_Syntax.NAT i) -> 
       (match (machine_int_of_string i) with
        | (ERROR_NAT _) -> RANDOM_AEXP
        | (NAT vi)      -> 
            let l = Array.make (n+1) 0 in l.(n) <- vi; 
              LINEAR_AEXP l)
   | (VAR v) -> (let l = Array.make (n+1) 0 in l.(v) <- 1; 
        LINEAR_AEXP l)
   | RANDOM -> RANDOM_AEXP
   | (UPLUS a1) -> (linearize_aexp a1)
   | (UMINUS a1) -> (match linearize_aexp a1 with
        | RANDOM_AEXP -> RANDOM_AEXP
        | LINEAR_AEXP l1 -> 
           let l = Array.make (n+1) 0 in
             (for i=0 to n do 
                match machine_unary_minus (NAT l1.(i)) with
                | ERROR_NAT _ -> raise Abstract_To_Linear_Syntax_error
                | NAT v -> l.(i) <- v
              done;
              LINEAR_AEXP l))
   | (PLUS (a1, a2))  -> 
      (match (linearize_aexp a1, linearize_aexp a2) with
       | (RANDOM_AEXP, _) | (_, RANDOM_AEXP) -> RANDOM_AEXP
       | (LINEAR_AEXP l1, LINEAR_AEXP l2) ->
          let l = Array.make (n+1) 0 in
           (for i=0 to n do 
             match machine_binary_plus (NAT l1.(i)) (NAT l2.(i)) with
             | ERROR_NAT _ -> raise Abstract_To_Linear_Syntax_error
             | NAT v -> l.(i) <- v
             done;
             LINEAR_AEXP l))
   | (MINUS (a1, a2)) -> 
       (match (linearize_aexp a1, linearize_aexp a2) with
        | (RANDOM_AEXP, _) | (_, RANDOM_AEXP) -> RANDOM_AEXP
        | (LINEAR_AEXP l1, LINEAR_AEXP l2) ->
           let l = Array.make (n+1) 0 in
            (for i=0 to n do 
              match machine_binary_minus (NAT l1.(i)) (NAT l2.(i)) with
              | ERROR_NAT _ -> raise Abstract_To_Linear_Syntax_error
              | NAT v -> l.(i) <- v
             done; 
             LINEAR_AEXP l))
   | (TIMES (a1, a2)) -> 
      (match (linearize_aexp a1, linearize_aexp a2) with
       | (RANDOM_AEXP, _) | (_, RANDOM_AEXP) -> RANDOM_AEXP
       | (LINEAR_AEXP l1, LINEAR_AEXP l2) ->
          try
           for i=0 to n-1 do if l1.(i)<>0 then raise Not_constant done;
            let l = Array.make (n+1) 0 in
             for i=0 to n do 
              match machine_binary_times (NAT l1.(n)) (NAT l2.(i)) with
              | ERROR_NAT _ -> raise Abstract_To_Linear_Syntax_error
              | NAT v -> l.(i) <- v
             done; 
             LINEAR_AEXP l
          with Not_constant ->
           try
            for i=0 to n-1 do if l2.(i)<>0 then raise Not_linear done;
             let l = Array.make (n+1) 0 in
              for i=0 to n do 
               match machine_binary_times (NAT l1.(i)) (NAT l2.(n)) with
               | ERROR_NAT _ -> raise Abstract_To_Linear_Syntax_error
               | NAT v -> l.(i) <- v
              done; 
              LINEAR_AEXP l
           with Not_linear -> RANDOM_AEXP)
   | (DIV (a1, a2)) -> 
      (match (linearize_aexp a1, linearize_aexp a2) with
       | (RANDOM_AEXP, _) | (_, RANDOM_AEXP) -> RANDOM_AEXP
       | (LINEAR_AEXP l1, LINEAR_AEXP l2) ->
          try
           for i=0 to n-1 do if l2.(i)<>0 then raise Not_constant done;
            if (l2.(n) = 0) then
               RANDOM_AEXP
            else
             let l = Array.make (n+1) 0 in
              for i=0 to n do
               match machine_binary_div (NAT l1.(i)) (NAT l2.(i)) with
               | ERROR_NAT _ -> raise Abstract_To_Linear_Syntax_error
               | NAT v -> l.(i) <- v
              done;
             LINEAR_AEXP l
          with Not_constant -> RANDOM_AEXP)
   | (MOD (a1, a2))   -> RANDOM_AEXP
    with Abstract_To_Linear_Syntax_error -> RANDOM_AEXP
(* Linearization of boolean operations *)
let rec linearize_bexp b =
  match b with
  | TRUE           -> LTRUE
  | FALSE          -> LFALSE
  | (EQ (a1, a2))  -> 
      (match (linearize_aexp a1), (linearize_aexp a2) with
       | (RANDOM_AEXP, _) | (_, RANDOM_AEXP) -> RANDOM_BEXP
       | (LINEAR_AEXP l1, LINEAR_AEXP l2) ->
           let t = Array.make ((number_of_variables ())+1) 0 in
             for i=0 to (number_of_variables ()) do 
	       t.(i) <- l2.(i) - l1.(i) 
	     done; 
             LEQ t)
| (LT (a1, a2))  -> 
      (match (linearize_aexp a1), 
       (linearize_aexp (MINUS (a2, (Abstract_Syntax.NAT "1")))) with
       | (RANDOM_AEXP, _) | (_, RANDOM_AEXP) -> RANDOM_BEXP
       | (LINEAR_AEXP l1, LINEAR_AEXP l2) ->
           let t = Array.make ((number_of_variables ())+1) 0 in
             for i=0 to (number_of_variables ()) do 
	       t.(i) <- l2.(i) - l1.(i)
	     done; 
             LGE t)
  | (AND (b1, b2)) -> 
      (match (linearize_bexp b1), (linearize_bexp b2) with
       | (LFALSE, _) | (_, LFALSE) -> LFALSE
       | (LTRUE, b) -> b 
       | (a, LTRUE) -> a
       | (RANDOM_BEXP, _) | (_, RANDOM_BEXP) -> RANDOM_BEXP
       | (LAND l1, LAND l2) -> LAND (l1@l2)
       | (LAND l, b) -> LAND (l@[b])
       | (b, LAND l) -> LAND (b::l)
       | (b1', b2') -> LAND [b1';b2'])
  | (OR (b1, b2))  ->
      (match (linearize_bexp b1), (linearize_bexp b2) with
       | (LTRUE, _) | (_, LTRUE) -> LTRUE
       | (LFALSE, b) -> b 
       | (a, LFALSE) -> a
       | (RANDOM_BEXP, _) | (_, RANDOM_BEXP) -> RANDOM_BEXP
       | (LOR l1, LOR l2) -> LOR (l1@l2)
       | (LOR l, b) -> LOR (l@[b])
       | (b, LOR l) -> LOR (b::l)
       | (b1', b2') -> LOR [b1';b2'])
(* Linearization of commands *)
let rec linearize_com c =
  match c with 
  | SKIP (l1, l2) -> (LSKIP (l1, l2))
  | ASSIGN (l1, v, a, l2) -> (LASSIGN (l1, v, (linearize_aexp a), l2))
  | SEQ (l1, cl, l2) -> (LSEQ (l1, (linearize_com_list cl), l2))
  | IF (l1, b, nb, ct, cf, l2) -> 
      (LIF (l1, (linearize_bexp b), (linearize_bexp nb),
                 (linearize_com ct), (linearize_com cf), l2))
  | WHILE (l1, b, nb, c, l2) -> 
      (LWHILE (l1, (linearize_bexp b), (linearize_bexp nb), 
                                      (linearize_com c), l2))
and linearize_com_list cl =
  match cl with
  | [] -> []
  | c :: cl' -> (linearize_com c) :: (linearize_com_list cl')
