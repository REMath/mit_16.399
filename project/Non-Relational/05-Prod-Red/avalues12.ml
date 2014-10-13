(* avalues12.ml *)
open Avalues1
open Avalues2
open Red12
(* reduced product *)
(*                 *)
(* ABSTRACT VALUES *)
(*                 *)
type t = Avalues1.t * Avalues2.t
(* gamma (a,b) =  Avalues1.gamma(a) /\   Avalues2.gamma(b) *)
(* infimum: bot () = alpha({}) *)
let bot () = reduce (Avalues1.bot (), Avalues2.bot ())
(* isbotempty () = gamma(bot ()) = {} *)
let isbotempty () = (Avalues1.isbotempty ()) || (Avalues2.isbotempty ())
(* uninitialization: initerr () = alpha({_O_i}) *)
let initerr () = reduce (Avalues1.initerr (), Avalues2.initerr ())
(* supremum: top () = alpha({_O_i, _O_a} U [min_int,max_int]) *)
let top () =  reduce (Avalues1.top (), Avalues2.top ())
(* least upper bound join: p q = alpha(gamma(p) U gamma(q)) *)
let join (v,w) (x,y) = reduce ((Avalues1.join v x), (Avalues2.join w y))
(* greatest lower bound meet p q = alpha(gamma(p) cap gamma(q)) *)
let meet (v,w) (x,y) = reduce ((Avalues1.meet v x), (Avalues2.meet w y))
(* approximation ordering: leq p q = gamma(p) subseteq gamma(q) *)
let leq (v,w) (x,y) = (Avalues1.leq v x) & (Avalues2.leq w y)
(* equality: eq p q = gamma(p) = gamma(q) *)
let eq (v,w) (x,y) = (Avalues1.eq v x) & (Avalues2.eq w y)
(* included in errors?: in_errors p = gamma(p) subseteq {_O_i, _O_a} *)
let in_errors (x, y) = (Avalues1.in_errors x) || (Avalues2.in_errors y)
(* printing *)
let print (x, y) =  
   (print_string "("; 
    Avalues1.print x;
    print_string ", "; 
    Avalues2.print y;
    print_string ")")
(*                       *)
(* ABSTRACT TRANSFORMERS *)
(*                       *)
(* forward abstract semantics of arithmetic expressions *)
(* f_NAT s = alpha({(machine_int_of_string s)})        *)
let f_NAT s = reduce (Avalues1.f_NAT s, Avalues2.f_NAT s)
(* f_RANDOM () = alpha([min_int, max_int]) *)
let f_RANDOM () = reduce (Avalues1.f_RANDOM (), Avalues2.f_RANDOM ())
(* f_UMINUS a = alpha({Ê(machine_unary_minus x) | x \in gamma(a)} }) *)
let f_UMINUS (x, y) = reduce (Avalues1.f_UMINUS x, Avalues2.f_UMINUS y)
(* f_UPLUS a = alpha(gamma(a)) *)
let f_UPLUS (x, y) = reduce (x, y)
(* f_BINARITH a b = alpha({Ê(machine_binary_binarith i j) |              *)
(*                                      i in gamma(a) /\ j \in gamma(b)} *)
let f_PLUS (a, b) (c, d) = reduce (Avalues1.f_PLUS a c, Avalues2.f_PLUS b d)
let f_MINUS (a, b) (c, d) = reduce (Avalues1.f_MINUS a c, Avalues2.f_MINUS b d)
let f_TIMES (a, b) (c, d) = reduce (Avalues1.f_TIMES a c, Avalues2.f_TIMES b d)
let f_DIV (a, b) (c, d) = reduce (Avalues1.f_DIV a c, Avalues2.f_DIV b d)
let f_MOD (a, b) (c, d) = reduce (Avalues1.f_MOD a c, Avalues2.f_MOD b d)
(* forward abstract semantics of boolean expressions                   *)
(* Are there integer values in gamma(u) equal to values in gamma(v)?   *)
(* f_LT p q = exists i in gamma(p) cap [min_int,max_int]:              *)
(*          exists j in gamma(q) cap [min_int,max_int]: machine_eq i j *)
let f_EQ (a, b) (c, d) = (Avalues1.f_EQ a c) & (Avalues2.f_EQ b d)
(* Are there integer values in gamma(u) strictly less than (<) integer *)
(* integer values in gamma(v)?                                         *)
(* f_LT p q = exists i in gamma(p) cap [min_int,max_int]:              *)
(*          exists j in gamma(q) cap [min_int,max_int]: machine_lt i j *)
let f_LT (a, b) (c, d) = (Avalues1.f_LT a c) & (Avalues2.f_LT b d)
(* widening *)
let widen (a, b) (c, d) = reduce (Avalues1.widen a c, Avalues2.widen b d)
(* narrowing *)
let narrow (a, b) (c, d) = reduce (Avalues1.narrow a c, Avalues2.narrow b d)
(* backward abstract semantics of arithmetic expressions                     *)
(* b_NAT s v = (machine_int_of_string s) in gamma(v) cap [min_int, max_int]? *)
let b_NAT s (a, b) = (Avalues1.b_NAT s a) & (Avalues2.b_NAT s b)
(* b_RANDOM p = gamma(p) cap [min_int, max_int]  <> emptyset *)
let b_RANDOM (a, b) = (Avalues1.b_RANDOM a) & (Avalues2.b_RANDOM b)
(* b_UOP q p = alpha({i in gamma(q) | UOP(i) \in gamma(p) cap [min_int, max_int]}) *)
let b_UMINUS (a, b) (c, d) = reduce (Avalues1.b_UMINUS a c, Avalues2.b_UMINUS b d)
let b_UPLUS (a, b) (c, d) = reduce (Avalues1.b_UPLUS a c, Avalues2.b_UPLUS b d)
(* b_BOP q1 q2 p = alpha2({<i1,i2> in gamma2(<q1,q2>) |                            *)
(*                               BOP(i1, i2) \in gamma(p) cap [min_int, max_int]}) *)
let b_PLUS  (a, b) (c, d) (e, f) = let (a', c') = Avalues1.b_PLUS a c e in 
                                    let (b', d') = Avalues2.b_PLUS b d f in 
                                     (reduce (a', b'), reduce (c', d'))
let b_MINUS (a, b) (c, d) (e, f) = let (a', c') = Avalues1.b_MINUS a c e in 
                                    let (b', d') = Avalues2.b_MINUS b d f in
                                     (reduce (a', b'), reduce (c', d'))
let b_TIMES (a, b) (c, d) (e, f) = let (a', c') = Avalues1.b_TIMES a c e in 
                                    let (b', d') = Avalues2.b_TIMES b d f in 
                                     (reduce (a', b'), reduce (c', d'))
let b_DIV   (a, b) (c, d) (e, f) = let (a', c') = Avalues1.b_DIV a c e in
                                    let (b', d') = Avalues2.b_DIV b d f in 
                                     (reduce (a', b'), reduce (c', d'))
let b_MOD   (a, b) (c, d) (e, f) = let (a', c') = Avalues1.b_MOD a c e in 
                                    let (b', d') = Avalues2.b_MOD b d f in 
                                     (reduce (a', b'), reduce (c', d'))
(* backward abstract interpretation of boolean expressions *)
(* a_EQ p1 p2 = let p = p1 cap p2 cap [min_int, max_int]I in <p, p> *)
let a_EQ p1 p2 = let p = meet p1 p2 in (p, p)
(* a_LT p1 p2 = alpha2({<i1, i2> | i1 in gamma(p1) cap [min_int, max_int] /\ *)
(*                                 i2 in gamma(p1) cap [min_int, max_int] /\ *)  
(*                                 i1 < i2})                                 *)
let a_LT (a, b) (c, d) = 
  let (a', c') = Avalues1.a_LT a c in
    let (b', d') =  Avalues2.a_LT b d in
     ((reduce (a', b')), (reduce (c', d')))
