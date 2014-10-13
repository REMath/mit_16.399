(* concrete_To_Abstract_Syntax.ml *)
open Abstract_Syntax
(* abstract syntax *)
type variable = Abstract_Syntax.variable
and aexp = Abstract_Syntax.aexp
and bexp = Abstract_Syntax.bexp
and label = Abstract_Syntax.label
and com = Abstract_Syntax.com
(* concrete syntax *)
type c_bexp =
| C_TRUE
| C_FALSE
| C_LT of aexp * aexp
| C_LEQ of aexp * aexp
| C_EQ of aexp * aexp
| C_NEQ of aexp * aexp
| C_GT of aexp * aexp
| C_GEQ of aexp * aexp
| C_OR of c_bexp * c_bexp
| C_AND of c_bexp * c_bexp
| C_NEG of c_bexp
type c_com =
| C_SKIP
| C_ASSIGN of variable * aexp
| C_SEQ of c_com list
| C_IF of c_bexp * c_com * c_com
| C_WHILE of c_bexp * c_com
 
(* normalization of boolean expressions *)
let rec tbexp b = match b with
| C_TRUE           -> TRUE
| C_FALSE          -> FALSE
| (C_LT  (v1, v2)) -> (LT (v1,v2))
| (C_LEQ (v1, v2)) -> (OR ((LT (v1,v2)),(EQ (v1,v2))))
| (C_EQ  (v1, v2)) -> (EQ (v1,v2))
| (C_NEQ (v1, v2)) -> (OR ((LT (v1,v2)),(LT (v2,v1))))
| (C_GT  (v1, v2)) -> (LT (v2,v1))
| (C_GEQ (v1, v2)) -> (OR ((LT (v2,v1)),(EQ (v1,v2))))
| (C_OR (b1,b2))   -> (OR ((tbexp b1),(tbexp b2)))
| (C_AND (b1,b2))  -> (AND ((tbexp b1),(tbexp b2)))
| (C_NEG b')       -> tnotbexp b'
and tnotbexp b = match b with
| C_TRUE           -> FALSE
| C_FALSE          -> TRUE
| (C_LT  (v1, v2)) -> tbexp (C_GEQ (v1, v2))
| (C_LEQ (v1, v2)) -> tbexp (C_GT (v1, v2))
| (C_EQ  (v1, v2)) -> tbexp (C_NEQ (v1, v2))
| (C_NEQ (v1, v2)) -> tbexp (C_EQ (v1, v2))
| (C_GT  (v1, v2)) -> tbexp (C_LEQ (v1, v2))
| (C_GEQ (v1, v2)) -> tbexp (C_LT (v1, v2))
| (C_OR  (b1,b2))  -> (AND ((tnotbexp b1),(tnotbexp b2)))
| (C_AND (b1,b2))  -> (OR ((tnotbexp b1),(tnotbexp b2)))
| (C_NEG b')       -> tbexp b'
  
(* variables of arithmetic expressions *)
let leq x y = x <= y
let rec reduce l = match l with
  (* eliminate duplicates in sorted list of variables *)
| [] -> []
| [v] -> [v]
|  h1 :: h2 :: t -> let q = (reduce (h2 :: t)) in
			    if (h1 = h2) then q else h1 :: q
let rec varaexp a = match a with
| (NAT i)          -> []
| (VAR v)          -> [v]
| RANDOM           -> []
| (UMINUS a1)      -> (varaexp a1)
| (UPLUS  a1)      -> (varaexp a1)
| (PLUS  (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
| (MINUS (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
| (TIMES (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
| (DIV   (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
| (MOD   (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
(* variables of boolean expressions *)
let rec varbexp b = match b with
| C_TRUE           -> []
| C_FALSE          -> []
| (C_LT  (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
| (C_LEQ (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
| (C_EQ  (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
| (C_NEQ (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
| (C_GT  (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
| (C_GEQ (a1, a2)) -> reduce (Sort.merge leq (varaexp a1) (varaexp a2))
| (C_OR  (b1,b2))  -> reduce (Sort.merge leq (varbexp b1) (varbexp b2))
| (C_AND (b1,b2))  -> reduce (Sort.merge leq (varbexp b1) (varbexp b2))
| (C_NEG b')       -> (varbexp b')

(* program labelling *)
let at c = match c with
| SKIP (l,m)           -> l
| ASSIGN (l,x,a,m)     -> l
| SEQ (l,s,m)          -> l
| IF (l,b,nb,st,sf,m)  -> l
| WHILE (l,b,nb,s,m)   -> l
let after c = match c with
| SKIP (l,m)           -> m
| ASSIGN (l,x,a,m)     -> m
| SEQ (l,s,m)          -> m
| IF (l,b,nb,st,sf,m)  -> m
| WHILE (l,b,nb,s,m)   -> m
let rec incom l c = match c with
| SKIP (l1,l2)          -> (l=l1) or (l=l2)
| ASSIGN (l1,x,a,l2)    -> (l=l1) or (l=l2)
| SEQ (l1,s,l2)         -> (l=l1) or (l=l2) or (inseq l s)
| IF (l1,b,nb,st,sf,l2) -> (l=l1) or (l=l2) or (incom l st) 
                                  or (incom l sf)
| WHILE (l1,b,nb,s,l2)  -> (l=l1) or (l=l2) or (incom l s)
and inseq l s = match s with
| [] -> false
| h::t -> (incom l h) or (inseq l t)
exception Error_label_normalize_com of string
let rec label_normalize_seq_from l s = match s with
| []  -> raise (Error_label_normalize_com "empty sequence of commands")
| [c] -> let m, c' = label_normalize_com_from l c in
		  m, [c']
| h :: t -> let m, h' = label_normalize_com_from l h in
		  let n, t' = label_normalize_seq_from m t in
		    n, (h' :: t')
and label_normalize_com_from l c = match c with
| C_SKIP           -> l+1, (SKIP (l, (l+1)))
| (C_ASSIGN (v,a)) -> l+1, (ASSIGN (l, v, a, (l+1)))
| (C_IF (b,t,f))   -> let m, t' = label_normalize_com_from (l+1) t in
			    let n, f' = label_normalize_com_from (m+1) f in
			      n+1, (IF (l, (tbexp b), (tnotbexp b), t', f', (n+1)))
| (C_SEQ s)        -> let m, s' = label_normalize_seq_from l s in
			    m, (SEQ (l, s', m))
| (C_WHILE (b,c))  -> let m, c' = label_normalize_com_from (l+1) c in
			    m+1, (WHILE  (l, (tbexp b), (tnotbexp b), c', (m+1)))
let last_label = ref 0
let label_normalize_com c = 
  let m, c' = label_normalize_com_from 0 c in 
	last_label := m;
	c'
let number_of_labels () = (!last_label + 1)
let entry () = 0
let exit () = !last_label
let print_label l = (print_int l)
let string_of_label l = (string_of_int l)
