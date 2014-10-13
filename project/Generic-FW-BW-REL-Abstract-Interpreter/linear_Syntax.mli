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
val at : lcom -> label            (* command entry label            *)
val after : lcom -> label         (* command exit label             *)
val incom : label -> lcom -> bool (* label in command               *)
