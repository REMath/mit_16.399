(* program_To_Abstract_Syntax.mli *)
open Abstract_Syntax
(* parsing and concrete to abstract syntax translation *)
exception Syntax_Error
val abstract_syntax_of_program : string -> com
