(* abexp_f.ml *)
open Abstract_Syntax
open Avalues
open Aenv
open Aaexp
open Pretty_Print
open Trace
(* Forward abstract interpretation of boolean operations *)
let do_trace_bexp b r = if trace_bexp () then
		 ( print_string "Bexp["; print_Bexp b; print_string "] = "; Aenv.print r; print_newline (); r) else r
let rec a_bexp b r =
  match b with
  | TRUE           -> do_trace_bexp b r
  | FALSE          -> do_trace_bexp b (Aenv.bot ())
  | (EQ (a1, a2))  -> let p1 = (a_aexp a1) in let p2 = (a_aexp a2) in
                         do_trace_bexp b (f_EQ p1 p2 r)
  | (LT (a1, a2))  -> let p1 = (a_aexp a1) in let p2 = (a_aexp a2) in
                         do_trace_bexp b (f_LT p1 p2 r)
  | (AND (b1, b2)) -> let r1 = (a_bexp b1 r) in let r2 = (a_bexp b2 r) in
                         do_trace_bexp b (Aenv.meet r1 r2)
  | (OR (b1, b2))  -> let r1 = (a_bexp b1 r) in let r2 = (a_bexp b2 r) in
                         do_trace_bexp b (Aenv.join r1 r2)
