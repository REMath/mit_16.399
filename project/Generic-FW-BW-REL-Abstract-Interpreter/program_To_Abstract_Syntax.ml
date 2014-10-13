(* program_To_Abstract_Syntax.ml *)
open Lexing
open Abstract_Syntax
exception Syntax_Error
let abstract_syntax_of_program f = 
   let input_channel = if f = "" then stdin else open_in f in 
      try 
          let lexbuf = Lexing.from_channel input_channel  in
            (Parser.n_Prog Lexer.token lexbuf)
      with
          | Failure s -> print_string s; print_newline ();
                         flush stdout;
                         raise Syntax_Error
          | Lexer.Eof -> print_string "lexical error\n";
                         flush stdout;
                         raise Syntax_Error
          | Parsing.Parse_error -> 
                         print_string "syntax error\n";
                         flush stdout;
                         raise Syntax_Error
