(* main.ml = main-iter-fw-bw.ml *)
open Program_To_Abstract_Syntax
open Labels
open Pretty_Print
open Lpretty_Print
open Abstract_To_Linear_Syntax
open Linear_Syntax
open Aenv
open Acom
open Bcom
let _ =
 let narrowing_limit = 100 in
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
         let rec iterate pre n = 
           (print_string "** Precondition:\n";
            print pre;
            let post = (acom p' pre (after p')) in
              (print_string "** Postcondition:\n";
               print post;
               let pre' = (bcom p' post (at p')) in
                 (if (Aenv.eq pre pre') then 
                    (print_string "stable precondition after "; 
						   print_int n;print_string " iteration(s).\n")
                  else if (n < narrowing_limit) then 
                    (print_string "unstable precondition after "; 
						   print_int n;print_string " iteration(s).\n";
                     iterate pre' (n+1))
                  else
                    (print_string "stable stopped "; print_int n;
						   print_string " iterations (narrowing).\n"))))
         in iterate (initerr ()) 1;
         quit ())
