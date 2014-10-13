(* fixpoint.ml *)
open Aenv
(* iteration of f from prefixpoint x with ordering c and widening w *)
let rec luis x c w f =
  print_string "luis: x =\n";
  print x;
  let x' = (f x) in
    print_string "luis: f(x) =\n";
    print x';
    if (c x' x) then 
      (print_string "luis: f(x) <= x, convergence\n";
       x)
    else 
      (let x'' = (w x x') in
       print_string "luis: x \\/ f(x) =\n";
       print x'';
       luis x'' c w f)
(* iteration of f from postfixpoint x with ordering c and narrowing n *)
let rec llis x c n f =
  print_string "llis: x =\n";
  print x;
    let x' = (f x) in
      print_string "llis: f(x) =\n";
      print x';
      let x'' = (n x x') in
        print_string "llis: x /\\ f(x) =\n";
        print x'';
        if (c x x'') then 
          (print_string "llis: x <= x /\\ f(x), convergence\n";
           x')
				else llis x'' c n f
(* lfp x c w n f : iterative computation of a c-postfixpoint of f *)
(* c-greater than or equal to the prefixpoint x (x <= f(x)) with  *)
(* widening w and narrowing n                                     *)
let lfp x c w n f = llis (luis x c w f) c n f
(* gfp x c n f : iterative computation of a c-postfixpoint of f   *)
(* c-less than or equal to the postfixpoint x (f(x) <= x) with    *)
(* narrowing n                                                    *)
let gfp x c n f = llis x c n f
