(* concrete_To_Abstract_Syntax.mli *)
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
val tbexp    : c_bexp -> bexp
    val tnotbexp : c_bexp -> bexp

(* labels *)
val at               : com   -> label       (* command entry label *)
val after            : com   -> label       (* command exit label  *)
val incom            : label -> com -> bool (* label in command    *)
val number_of_labels : unit  -> int
val entry            : unit  -> label       (* program entry label *)
val exit             : unit  -> label       (* program exit label  *)
val print_label      : label -> unit
val string_of_label  : label -> string

(* program labelling *)
val label_normalize_com : c_com -> com 
