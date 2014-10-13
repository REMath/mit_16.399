type token =
  | T_NAT of (string)
  | T_LPAR
  | T_RPAR
  | T_RANDOM
  | T_PLUS
  | T_MINUS
  | T_TIMES
  | T_DIV
  | T_MOD
  | T_TRUE
  | T_FALSE
  | T_LT
  | T_LEQ
  | T_EQ
  | T_NEQ
  | T_GT
  | T_GEQ
  | T_OR
  | T_AND
  | T_NEG
  | T_SKIP
  | T_ASSIGN
  | T_SEQ
  | T_IF
  | T_THEN
  | T_ELSE
  | T_FI
  | T_WHILE
  | T_DO
  | T_OD
  | T_AINITIAL
  | T_FINAL
  | T_ALWAYS
  | T_SOMETIME
  | T_VAR of (string)
  | T_EOP

val n_Prog :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Abstract_Syntax.com
