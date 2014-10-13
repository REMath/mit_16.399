(* main.ml *)
open Program_To_Abstract_Syntax
open Labels
open Pretty_Print
open Aenv
open Acom
let _ =
  let arg = if (Array.length  Sys.argv) = 1 then ""
            else Sys.argv.(1) in 
     Random.self_init ();
	 let p = (abstract_syntax_of_program arg) in
		(print (initerr ());
		 pretty_print p;
		 print (acom p (initerr ()) (after p));
		 print_newline ())
