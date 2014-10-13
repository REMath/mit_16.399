(* main.ml *)
open Program_To_Abstract_Syntax
open Pretty_Print
let _ =
  let arg = Sys.argv.(1) in 
    let p = (abstract_syntax_of_program arg) in
      pretty_print p
