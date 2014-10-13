(* fixpoint.ml *)
(* lfp x c f : iterative computation of the c-least fixpoint of f *)
(* c-greater than or equal to the prefixpoint x (f(x) >= x)       *)
let rec lfp x c f =
  let x' = (f x) in
    if (c x' x) then x'
      else lfp x' c f
