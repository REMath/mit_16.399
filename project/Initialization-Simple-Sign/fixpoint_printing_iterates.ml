(* fixpoint.ml *)
open Aenv
(* lfp x c f : iterative computation of the c-least fixpoint of f *)
(* c-greater than or equal to the prefixpoint x (f(x) >= x)       *)
(* x0 = x; ...; xn+1 = xn U f(xn);... ; xl where xl c xl U f(xl)  *)
let lfp x c f =
  let rec iterate n x =
    print_string "iterate ";print_int n;print_string " = ";
    Aenv.print x; print_newline ();
   let x' = (join x (f x)) in
     (if (c x' x) then (print_string "fixpoint = ";
                        Aenv.print x'; 
                        print_newline ();x')
                 else iterate (n + 1) x')
  in iterate 0 x
