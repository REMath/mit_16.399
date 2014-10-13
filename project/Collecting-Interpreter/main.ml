(* main.ml *)
open Program_To_Abstract_Syntax
open Labels
open Pretty_Print
open Cenv
open Ccom
let _ =
  let arg = if (Array.length  Sys.argv) = 1 then ""
            else Sys.argv.(1) in 
     Random.self_init ();
	 let p = (abstract_syntax_of_program arg) in
		(print (initerr ());
		 pretty_print p;
		 print (ccom p (initerr ()) (after p));
		 print_newline ())
