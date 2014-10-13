(* avalues123.ml *)
open Avalues1
open Avalues2
open Avalues3
open Red123
(* reduced product *)
(*                 *)
(* ABSTRACT VALUES *)
(*                 *)
type t = Avalues1.t * Avalues2.t * Avalues3.t
(* gamma (a,b,c) =  Avalues1.gamma(a) /\ Avalues2.gamma(b) /\ Avalues2.gamma(c) *)
(* infimum: bot () = alpha({}) *)
let bot () = reduce ((Avalues1.bot ()), (Avalues2.bot ()), (Avalues3.bot ()))
(* isbotempty () = gamma(bot ()) = {} *)
let isbotempty () = (Avalues1.isbotempty ()) || (Avalues2.isbotempty ()) || (Avalues3.isbotempty ())
(* uninitialization: initerr () = alpha({_O_i}) *)
let initerr () = reduce ((Avalues1.initerr ()), (Avalues2.initerr ()), (Avalues3.initerr ()))
(* supremum: top () = alpha({_O_i, _O_a} U [min_int,max_int]) *)
let top () =  reduce (Avalues1.top (), Avalues2.top (), Avalues3.top ())
(* least upper bound join: p q = alpha(gamma(p) U gamma(q)) *)
let join (v,w,t) (x,y,u) = reduce ((Avalues1.join v x), (Avalues2.join w y), (Avalues3.join t u))
(* greatest lower bound meet p q = alpha(gamma(p) cap gamma(q)) *)
let meet (v,w,t) (x,y,u) = reduce ((Avalues1.meet v x), (Avalues2.meet w y), (Avalues3.meet t u))
(* approximation ordering: leq p q = gamma(p) subseteq gamma(q) *)
let leq (v,w,t) (x,y, u) = (Avalues1.leq v x) & (Avalues2.leq w y) & (Avalues3.leq t u)
(* equality: eq p q = gamma(p) = gamma(q) *)
let eq (v,w,t) (x,y,u) = (Avalues1.eq v x) & (Avalues2.eq w y) & (Avalues3.eq t u)
(* included in errors?: in_errors p = gamma(p) subseteq {_O_i, _O_a} *)
let in_errors (x,y,z) = (Avalues1.in_errors x) || (Avalues2.in_errors y) || (Avalues3.in_errors z)
(* printing *)
let print (x,y,z) =  
   (print_string "("; 
    Avalues1.print x;
    print_string ","; 
    Avalues2.print y;
    print_string ","; 
    Avalues3.print z;
    print_string ")")
(*                       *)
(* ABSTRACT TRANSFORMERS *)
(*                       *)
(* forward abstract semantics of arithmetic expressions *)
(* f_NAT s = alpha({(machine_int_of_string s)})        *)
let f_NAT s = reduce (Avalues1.f_NAT s, Avalues2.f_NAT s, Avalues3.f_NAT s)
(* f_RANDOM () = alpha([min_int, max_int]) *)
let f_RANDOM () = reduce (Avalues1.f_RANDOM (), Avalues2.f_RANDOM (), Avalues3.f_RANDOM ())
(* f_UMINUS a = alpha({Ê(machine_unary_minus x) | x \in gamma(a)} }) *)
let f_UMINUS (x, y, z) = reduce (Avalues1.f_UMINUS x, Avalues2.f_UMINUS y, Avalues3.f_UMINUS z)
(* f_UPLUS a = alpha(gamma(a)) *)
let f_UPLUS (x, y, z) = reduce (x, y, z)
(* f_BINARITH a b = alpha({Ê(machine_binary_binarith i j) |              *)
(*                                      i in gamma(a) /\ j \in gamma(b)} *)
let f_PLUS (a, b, c) (d, e, f) = reduce (Avalues1.f_PLUS a d, Avalues2.f_PLUS b e, Avalues3.f_PLUS c f)
let f_MINUS (a, b, c) (d, e, f) = reduce (Avalues1.f_MINUS a d, Avalues2.f_MINUS b e, Avalues3.f_MINUS c f)
let f_TIMES (a, b, c) (d, e, f) = reduce (Avalues1.f_TIMES a d, Avalues2.f_TIMES b e, Avalues3.f_TIMES c f)
let f_DIV (a, b, c) (d, e, f) = reduce (Avalues1.f_DIV a d, Avalues2.f_DIV b e, Avalues3.f_DIV c f)
let f_MOD (a, b, c) (d, e, f) = reduce (Avalues1.f_MOD a d, Avalues2.f_MOD b e, Avalues3.f_MOD c f)
(* forward abstract semantics of boolean expressions                   *)
(* Are there integer values in gamma(u) equal to values in gamma(v)?   *)
(* f_LT p q = exists i in gamma(p) cap [min_int,max_int]:              *)
(*          exists j in gamma(q) cap [min_int,max_int]: machine_eq i j *)
let f_EQ (a, b, c) (d, e, f) = (Avalues1.f_EQ a d) & (Avalues2.f_EQ b e) & (Avalues3.f_EQ  c f)
(* Are there integer values in gamma(u) strictly less than (<) integer *)
(* integer values in gamma(v)?                                         *)
(* f_LT p q = exists i in gamma(p) cap [min_int,max_int]:              *)
(*          exists j in gamma(q) cap [min_int,max_int]: machine_lt i j *)
let f_LT (a, b, c) (d, e, f) = (Avalues1.f_LT a d) & (Avalues2.f_LT b e) & (Avalues3.f_LT c f)
(* widening *)
let widen (a, b, c) (d, e, f) = reduce (Avalues1.widen a d, Avalues2.widen b e, Avalues3.widen c f)
(* narrowing *)
let narrow (a, b, c) (d, e, f) = reduce (Avalues1.narrow a d, Avalues2.narrow b e, Avalues3.narrow c f)
(* backward abstract semantics of arithmetic expressions                     *)
(* b_NAT s v = (machine_int_of_string s) in gamma(v) cap [min_int, max_int]? *)
let b_NAT s (a, b, c) = (Avalues1.b_NAT s a) & (Avalues2.b_NAT s b) & (Avalues3.b_NAT s c)
(* b_RANDOM p = gamma(p) cap [min_int, max_int]  <> emptyset *)
let b_RANDOM (a, b, c) = (Avalues1.b_RANDOM a) & (Avalues2.b_RANDOM b) & (Avalues3.b_RANDOM c)
(* b_UOP q p = alpha({i in gamma(q) | UOP(i) \in gamma(p) cap [min_int, max_int]}) *)
let b_UMINUS (a, b, c) (d, e, f) = reduce (Avalues1.b_UMINUS a d, Avalues2.b_UMINUS b e, Avalues3.b_UMINUS c f)
let b_UPLUS (a, b, c) (d, e, f) = reduce (Avalues1.b_UPLUS a d, Avalues2.b_UPLUS b e, Avalues3.b_UPLUS c f)
(* b_BOP q1 q2 p = alpha2({<i1,i2> in gamma2(<q1,q2>) |                            *)
(*                               BOP(i1, i2) \in gamma(p) cap [min_int, max_int]}) *)
let b_PLUS (a, b, c) (d, e, f) (g, h, i) = 
  let (a', d') = Avalues1.b_PLUS a d g in 
   let (b', e') = Avalues2.b_PLUS b e h in 
    let (c', f') = Avalues3.b_PLUS c f i in 
       ((reduce (a', b', c')), (reduce (d', e', f')))
let b_MINUS (a, b, c) (d, e, f) (g, h, i) = 
  let (a', d') = Avalues1.b_MINUS a d g in 
   let (b', e') = Avalues2.b_MINUS b e h in 
    let (c', f') = Avalues3.b_MINUS c f i in 
      ((reduce (a', b', c')), (reduce (d', e', f')))
let b_TIMES (a, b, c) (d, e, f) (g, h, i) = 
  let (a', d') = Avalues1.b_TIMES a d g in 
   let (b', e') = Avalues2.b_TIMES b e h in 
    let (c', f') = Avalues3.b_TIMES c f i in 
      ((reduce (a', b', c')), (reduce (d', e', f')))
let b_DIV (a, b, c) (d, e, f) (g, h, i) = 
  let (a', d') = Avalues1.b_DIV a d g in 
   let (b', e') = Avalues2.b_DIV b e h in 
    let (c', f') = Avalues3.b_DIV c f i in 
      ((reduce (a', b', c')), (reduce (d', e', f')))
let b_MOD (a, b, c) (d, e, f) (g, h, i) = 
  let (a', d') = Avalues1.b_MOD a d g in 
   let (b', e') = Avalues2.b_MOD b e h in 
    let (c', f') = Avalues3.b_MOD c f i in 
      ((reduce (a', b', c')), (reduce (d', e', f')))
(* backward abstract interpretation of boolean expressions *)
(* a_EQ p1 p2 = let p = p1 cap p2 cap [min_int, max_int]I in <p, p> *)
let a_EQ p1 p2 = let p = meet p1 p2 in (p, p)
(* a_LT p1 p2 = alpha2({<i1, i2> | i1 in gamma(p1) cap [min_int, max_int] /\ *)
(*                                 i2 in gamma(p1) cap [min_int, max_int] /\ *)  
(*                                 i1 < i2})                                 *)
let a_LT (a, b, c) (d, e, f) = 
  let (a', d') = Avalues1.a_LT a d in
   let (b', e') =  Avalues2.a_LT b e in
    let (c', f') =  Avalues3.a_LT c f in
     ((reduce (a', b', c')), (reduce (d', e', f')))
