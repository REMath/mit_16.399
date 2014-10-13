{ (* from file lexer.mll (lexical analysis) *)
open Parser            (* The type token is defined in parser.mli *)
exception Eof
}
rule token = parse
  [' ' '\t' '\n' '\r'] { token lexbuf }
| ( '%' [^'%']* '%')   { token lexbuf }
| ['0'-'9']+           { (T_NAT (Lexing.lexeme lexbuf)) }
| '('                  { T_LPAR }
| ')'                  { T_RPAR }
| '?'                  { T_RANDOM }
| '+'                  { T_PLUS }
| '-'                  { T_MINUS }
| '*'                  { T_TIMES }
| '/'                  { T_DIV }
| "mod"                { T_MOD }
| "true"               { T_TRUE }
| "false"              { T_FALSE }
| '<'                  { T_LT }
| "<="                 { T_LEQ }
| '='                  { T_EQ }
| "<>"                 { T_NEQ }
| '>'                  { T_GT }
| ">="                 { T_GEQ }
| 'Â'                  { T_NEG }
| '|'                  { T_OR }
| '&'                  { T_AND }
| "skip"               { T_SKIP }
| ":="                 { T_ASSIGN }
| ';'                  { T_SEQ }
| "if"                 { T_IF }
| "then"               { T_THEN }
| "else"               { T_ELSE }
| "fi"                 { T_FI }
| "while"              { T_WHILE }
| "do"                 { T_DO }
| "od"                 { T_OD }
| (['a'-'z'] | ['A'-'Z'])(['a'-'z'] | ['A'-'Z'] | ['0'-'9'])*
                       { (T_VAR (Lexing.lexeme lexbuf)) }
| ";;"                 { T_EOP }
| eof                  { raise Eof }
