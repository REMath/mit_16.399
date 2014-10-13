(* fixpoint.ml *)
open Aenv
(* lfp x c f : iterative computation of the c-least fixpoint of f *)
(* c-greater than or equal to the prefixpoint x (f(x) >= x)       *)
let lfp x c f =
  let rec iterate n x =
    print_string "iterate ";print_int n;print_string " = ";print x; 
    print_newline ();
    let x' = (f x) in
      (if (c x' x) then (print_string "fixpoint (at iterate ";
                         print_int n; print_string ") = ";print x'; 
		         print_newline ();x')
	           else iterate (n + 1) x')
    in iterate 0 x
let gfp x c f =
  let c_1 a b = c b a in
    lfp x c_1 f
    
