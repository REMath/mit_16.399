(* fixpoint.ml *)
open Aenv
(* lfp x c f : iterative computation of the c-least fixpoint of f *)
(* c-greater than or equal to the prefixpoint x (f(x) >= x)       *)
let rec lfp x c f =
  let x' = (f x) in
    if (c x' x) then x'
      else lfp x' c f
(* gfp x c f : iterative computation of the c-greatest fixpoint of *)
(* f, c-less than or equal to the postfixpoint x (f(x) <= x)       *)
let gfp x c f =
  let c_1 a b = c b a in
    lfp x c_1 f
