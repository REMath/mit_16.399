(* fixpoint.ml *)
open Cenv
(* lfp x c f : iterative computation of the c-least fixpoint of f *)
(* c-greater than or equal to the prefixpoint x (f(x) >= x)       *)
(* x0 = x; ...; xn+1 = xn U f(xn);... ; xl where xl c xl U f(xl)  *)
let rec lfp x c f =
  let x' = (join x (f (copy x))) in
    if (c x' x) then x'
      else lfp x' c f
