(* red123.ml *)
open Red12 
open Red23
open Red13
open Avalues1
open Avalues2
open Avalues3
open Trace
(* printing *)
let print (x,y,z) =  
   (print_string "("; 
    Avalues1.print x;
    print_string ","; 
    Avalues2.print y;
    print_string ","; 
    Avalues3.print z;
    print_string ")")
let reduce' (a, b, c) =
 let (a', b') = Red12.reduce (a, b) in
   let (b'', c') = Red23.reduce (b', c) in
     let (a'', c'') = Red13.reduce (a', c') in
       (a'', b'', c'')
let rec reduce t = 
  if trace_red () then (print t; print_string " -> "); 
  let t' = (reduce' t) in
    if (t = t') then (if trace_red () then (print_string "stable\n"); t)
    else (reduce t')

 
