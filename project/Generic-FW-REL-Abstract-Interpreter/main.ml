(* main.ml *)
open Program_To_Abstract_Syntax
open Labels
open Pretty_Print
open Lpretty_Print
open Abstract_To_Linear_Syntax
open Linear_Syntax
open Aenv
open Acom
let _ =
  let arg = if (Array.length  Sys.argv) = 1 then ""
            else Sys.argv.(1) in 
     Random.self_init ();
	 let p = (abstract_syntax_of_program arg) in
		(print_string "** Program:\n";
		 pretty_print p;
       let p' = (linearize_com p) in
         print_string "** Linearized program:\n";
         lpretty_print p';
         init ();
         print_string "** Precondition:\n";
         print (initerr ());
         print_string "** Postcondition:\n";
		   print (acom p' (initerr ()) (after p'));
         quit ())
