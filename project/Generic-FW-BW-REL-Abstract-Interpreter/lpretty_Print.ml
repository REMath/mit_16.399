(* lpretty_Print.ml *)
open Linear_Syntax
open Variables
open Labels
(* print linearized arithmetic expressions *)
let rec print_Laexp a = match a with
| RANDOM_AEXP -> 
    (print_string "?")
| LINEAR_AEXP l ->
    (let print_var v = (print_int l.(v); print_string "."; 
	                                             print_variable v)
     and print_plus v = (print_string " + ")
     in (map_variables print_var print_plus;
         print_string " + ";  
         print_int l.(number_of_variables ())))
(* print linear boolean expressions *)
let rec print_Lbexp b = match b with
| LTRUE          -> print_string "true"
| LFALSE         -> print_string "false"
| RANDOM_BEXP    -> print_string "??"
| (LGE l)        -> (let print_var v = (print_int l.(v);
                              print_string "."; print_variable v)
                     and print_plus v = (print_string " + ")
                     in (map_variables print_var print_plus;
                         print_string " + "; 
                         print_int l.(number_of_variables ());
                         print_string " >= 0"))
| (LEQ l)        -> (let print_var v = (print_int l.(v); 
                              print_string "."; print_variable v)
                     and print_plus v = (print_string " + ")
                     in (map_variables print_var print_plus;
                         print_string " + "; 
                         print_int l.(number_of_variables ());
                         print_string " = 0"))
| (LOR  bl)      -> print_string "("; (print_Lbexp_or bl);
                                                 print_string ")" 
| (LAND bl)      -> print_string "("; (print_Lbexp_and bl); 
                                                 print_string ")" 
and print_Lbexp_or bl = match bl with
| []       -> ()
| b :: []  -> print_Lbexp b
| b :: bl' -> print_Lbexp b; print_string " | "; print_Lbexp_or bl'
and print_Lbexp_and bl = match bl with
| []       -> ()
| b :: []  -> print_Lbexp b
| b :: bl' -> print_Lbexp b; print_string " & "; print_Lbexp_or bl'
exception Error_lpretty_print of string
(* print linearized program *)		
let lpretty_print c =
  let rec print_margin n = 
    if n > 0 then (print_string "  "; print_margin (n-1))
    else () 
  and print_margin_label n l = 
    (print_margin n;
     print_label l; print_string ": ";
     print_newline ())
  and print_seq n s = 
    match s with
    | []      -> raise (Error_lpretty_print 
	                                 "empty sequence of commands")
    | [c']    -> print_com n c'
    | h :: s' -> (print_com n h;
                 print_string ";"; print_newline ();
                  print_seq n s')
  and print_com n c' = 
    match c' with
    | (LSKIP (l,m)) -> 
        print_margin_label n l; print_margin (n+1);
        print_string "skip"
    | (LASSIGN (l,v,a,m)) -> 
       print_margin_label n l; 
       print_margin (n+1); print_variable v; print_string " := "; 
       print_Laexp a
    | (LSEQ (l,s,m)) -> 
        print_seq n s
    | (LIF (l,b,nb,t,f,m)) ->
        print_margin_label n l; print_margin (n+1);
        print_string "if "; print_Lbexp b;
        print_string " then"; print_newline();
        print_com_line (n+2) t;
        print_margin (n+1); print_string "else";
        print_string " {"; print_Lbexp nb; print_string "}";
        print_newline ();
        print_com_line (n+2) f;
        print_margin (n+1); print_string "fi"
    | (LWHILE (l,b,nb,c'',m))  ->
        print_margin_label n l; print_margin (n+1);
        print_string "while "; print_Lbexp b; 
        print_string " do"; print_newline();
        print_com_line (n+2) c'';
        print_margin (n+1); print_string "od";
        print_string " {"; print_Lbexp nb; print_string "}"
  and print_com_line n c' = 
     print_com n c'; print_newline ();
     print_margin_label n (Linear_Syntax.after c')
  in
     print_com_line 0 c
