(* fixpoint.ml *)
open Aenv
(* iteration of f from a prefixpoint x with ordering c and widening w *)
let rec luis x c w f =
  let x' = (f x) in
    if (c x' x) then 
       x
    else 
      (luis (w x x') c w f)
(* iteration of f from a postfixpoint x with ordering c and narrowing n *)
let rec llis x c n f =
  let x' = (f x) in
    let x'' = (n x x') in
      if (c x x'') then 
          x'
		  else 
         (llis x'' c n f)
(* lfp x c w n f : iterative computation of a c-postfixpoint of f *)
(* c-greater than or equal to the prefixpoint x (x <= f(x)) with  *)
(* widening w and narrowing n                                     *)
let lfp x c w n f = llis (luis x c w f) c n f
(* gfp x c n f : iterative computation of a c-postfixpoint of f   *)
(* c-less than or equal to the postfixpoint x (f(x) <= x) with    *)
(* narrowing n                                                    *)
let gfp x c n f = llis x c n f
