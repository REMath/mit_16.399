(* main.ml *)
open Program_To_Abstract_Syntax
open Pretty_Print
open Bigstep
(* read, parse and execute the program *)
let _ =
  let arg = if (Array.length  Sys.argv) = 1 then ""
            else Sys.argv.(1) in 
     Random.self_init ();
	 let p = (abstract_syntax_of_program arg) in
		(pretty_print p;
		 run p)
