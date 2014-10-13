(* abexp_fb.ml *)
open Abstract_Syntax
open Pretty_Print
open Trace
(* Forward/backward abstract interpretation of boolean operations *)
let do_trace_bexp b r r' = if trace_bexp () then
	 (print_string "Bexp["; print_Bexp b; print_string "]("; Aenv.print r; print_string ") = ";  Aenv.print r'; print_newline (); r') else r'
let do_trace_aexp_value a r p = if trace_bexp () then
   (print_string "Aexp["; print_Aexp a; print_string "]("; Aenv.print r; print_string ") = "; Avalues.print p; print_newline ()) else ()
let rec a_bexp' b r = 
      match b with
      | TRUE           -> do_trace_bexp b r r
      | FALSE          -> do_trace_bexp b r (Aenv.bot ())
      | (EQ (a1, a2))  ->
          let (p1,p2) = (Avalues.a_EQ (Aaexp.a_aexp a1 r) (Aaexp.a_aexp a2 r))
            in (do_trace_aexp_value a1 r p1;
						    do_trace_aexp_value a2 r p2;
								let b1 = (Baexp.b_aexp a1 r p1) in
								  let b2 = (Baexp.b_aexp a2 r p2) in
						        do_trace_bexp b r (Aenv.meet b1 b2))
      | (LT (a1, a2))  ->
          let (p1,p2) = (Avalues.a_LT (Aaexp.a_aexp a1 r) (Aaexp.a_aexp a2 r))
            in (do_trace_aexp_value a1 r p1;
						    do_trace_aexp_value a2 r p2;
                let b1 = (Baexp.b_aexp a1 r p1) in
                  let b2 = (Baexp.b_aexp a2 r p2) in
                    do_trace_bexp b r (Aenv.meet b1 b2))
      | (AND (b1, b2)) -> do_trace_bexp b r (let r1 = (a_bexp' b1 r) in let r2 = (a_bexp' b2 r) 
			                                         in Aenv.meet r1 r2)
      | (OR (b1, b2))  -> do_trace_bexp b r (let r1 = (a_bexp' b1 r) in let r2 = (a_bexp' b2 r) 
			                                         in Aenv.join r1 r2)
let a_bexp b r =
   if (Aenv.is_bot r) & (Avalues.isbotempty ()) 
   then (do_trace_bexp b r (Aenv.bot ())) else (a_bexp' b r)
