(* linear_Syntax.mli *)
open Abstract_Syntax   
(* A linear arithmetic expression a1.x1+...+an.xn+b, where n is the *)
(* number of program variables, is represented by a vector:         *)
(* LINEAR_AEXP a1 ... an b. A non-linear arithmetic expression is   *)
(* represented by RANDOM_AEXP.                                      *)
type laexp =
  | RANDOM_AEXP              (* random expression                   *)
  | LINEAR_AEXP of int array (* linear expression                   *)
and lbexp =
  | LTRUE | LFALSE     (* constant boolean expression               *)
  | RANDOM_BEXP        (* random boolean expression                 *)
  | LAND of lbexp list (* boolean conjunction                       *)
  | LOR  of lbexp list (* boolean disjunction                       *)
  | LGE  of int array  (* LGE a1 ... an b is a1.x1+...+an.xn >= b   *)
  | LEQ  of int array  (* LGE a1 ... an b is a1.x1+...+an.xn = b    *)
and label = Abstract_Syntax.label
and lcom =
  | LSKIP of label * label
  | LASSIGN of label * variable * laexp * label
  | LSEQ of label * (lcom list) * label
  | LIF of label * lbexp * lbexp * lcom * lcom * label
  | LWHILE of label * lbexp * lbexp * lcom * label
(* linearized command entry label *)
let at c = match c with
  | LSKIP (l,m)           -> l
  | LASSIGN (l,x,a,m)     -> l
  | LSEQ (l,s,m)          -> l
  | LIF (l,b,nb,st,sf,m)  -> l
  | LWHILE (l,b,nb,s,m)   -> l
(* linearized command exit label *)
let after c = match c with
  | LSKIP (l,m)           -> m
  | LASSIGN (l,x,a,m)     -> m
  | LSEQ (l,s,m)          -> m
  | LIF (l,b,nb,st,sf,m)  -> m
  | LWHILE (l,b,nb,s,m)   -> m
(* label in command    *)
let rec incom l c = match c with
  | LSKIP (l1,l2)          -> (l=l1) or (l=l2)
  | LASSIGN (l1,x,a,l2)    -> (l=l1) or (l=l2)
  | LSEQ (l1,s,l2)         -> (l=l1) or (l=l2) or (inseq l s)
  | LIF (l1,b,nb,st,sf,l2) -> (l=l1) or (l=l2) or (incom l st)
                                               or (incom l sf)
  | LWHILE (l1,b,nb,s,l2)  -> (l=l1) or (l=l2) or (incom l s)
and inseq l s = match s with
  | [] -> false
  | h::t -> (incom l h) or (inseq l t)
