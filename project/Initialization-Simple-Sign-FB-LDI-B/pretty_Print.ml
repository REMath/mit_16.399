(* pretty_Print.ml *)
open Abstract_Syntax
open Variables
open Labels
(* print arithmetic expressions *)
let rec print_Aexp a = match a with
  (NAT i)  -> 
    print_string i
| RANDOM -> 
    print_string "?"
| (VAR v) -> 
    (print_variable v)
| (PLUS  (b',b'')) -> 
    print_string "("; print_Aexp b'; 
    print_string " + "; print_Aexp b''; 
    print_string ")"
| (MINUS (b',b'')) -> 
    print_string "("; print_Aexp b'; 
    print_string " - "; print_Aexp b''; 
    print_string ")"
| (TIMES (b',b'')) -> 
    print_string "("; print_Aexp b';  
    print_string " * "; print_Aexp b'';  
    print_string ")"
| (DIV   (b',b'')) -> 
    print_string "("; print_Aexp b'; 
    print_string " / "; print_Aexp b'';  
    print_string ")"
| (MOD (b',b'')) -> 
    print_string "("; print_Aexp b';  
    print_string " mod "; print_Aexp b'';  
    print_string ")"
| (UMINUS b') -> 
    print_string "-"; print_Aexp b'
| (UPLUS b') -> 
    print_string "+"; print_Aexp b'
(* print boolean expressions *)
let rec print_Bexp b = match b with
| TRUE           -> print_string "true"
| FALSE          -> print_string "false"
| (EQ  (a,a'))   -> print_string "("; print_Aexp a; 
                    print_string " = ";   
                    print_Aexp a'; print_string ")"
| (LT  (a,a'))   -> print_string "("; print_Aexp a; 
                    print_string " < "; 
                    print_Aexp a'; print_string ")"
| (OR  (b',b'')) -> print_string "("; print_Bexp b'; 
                    print_string " | "; 
                    print_Bexp b''; print_string ")"  
| (AND (b',b'')) -> print_string "("; print_Bexp b'; 
                    print_string " & ";   
                    print_Bexp b''; print_string ")"
exception Error_pretty_print of string
(* print program with local abstract environments *)		
let pretty_print c =
  let rec print_margin n = 
    if n > 0 then (print_string "  "; print_margin (n-1))
    else () 
  and print_margin_label n l = 
    (print_margin n;
     print_label l; print_string ": ";
	  print_newline ())
  and print_seq n s = 
    match s with
    | []      -> raise (Error_pretty_print "empty sequence of commands")
    | [c']    -> print_com n c'
    | h :: s' -> (print_com n h;
	              print_string ";"; print_newline ();
                  print_seq n s')
  and print_com n c' = 
    match c' with
    | (SKIP (l,m)) -> 
        print_margin_label n l; print_margin (n+1);
        print_string "skip"
    | (ASSIGN (l,v,a,m)) -> 
       print_margin_label n l; 
	    print_margin (n+1); print_variable v; print_string " := "; 
	    print_Aexp a
    | (SEQ (l,s,m)) -> 
        print_seq n s
    | (IF (l,b,nb,t,f,m)) ->
        print_margin_label n l; print_margin (n+1);
        print_string "if "; print_Bexp b;
        print_string " then"; print_newline();
        print_com_line (n+2) t;
        print_margin (n+1); print_string "else";
        print_string " {"; print_Bexp nb; print_string "}";
        print_newline ();
        print_com_line (n+2) f;
        print_margin (n+1); print_string "fi"
    | (WHILE (l,b,nb,c'',m))  ->
        print_margin_label n l; print_margin (n+1);
        print_string "while "; print_Bexp b; 
        print_string " do"; print_newline();
        print_com_line (n+2) c'';
        print_margin (n+1); print_string "od";
        print_string " {"; print_Bexp nb; print_string "}"
  and print_com_line n c' = 
     print_com n c'; print_newline ();
	  print_margin_label n (after c')
  in
     print_newline ();
     print_com_line 0 c;
	  print_newline()
