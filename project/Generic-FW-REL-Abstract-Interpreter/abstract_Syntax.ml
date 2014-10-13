(* abstract_Syntax.mli *)
type variable = int
and aexp =
  | NAT of string
  | VAR of variable
  | RANDOM
  | UMINUS of aexp
  | UPLUS of aexp
  | PLUS of aexp * aexp
  | MINUS of aexp * aexp
  | TIMES of aexp * aexp
  | DIV of aexp * aexp
  | MOD of aexp * aexp
and bexp =
  | TRUE
  | FALSE
  | EQ of aexp * aexp
  | LT of aexp * aexp
  | AND of bexp * bexp
  | OR of bexp * bexp
and label = int
and com =
  | SKIP of label * label
  | ASSIGN of label * variable * aexp * label
  | SEQ of label * (com list) * label
  | IF of label * bexp * bexp * com * com * label
  | WHILE of label * bexp * bexp * com * label
