(* bigstep.ml *)
open Abstract_Syntax
open Labels
open Values
open Env
open Smallstep

(* big-step operational semantics of commands *)
let run p =
  let rec exec (l, r) =
    if l = (exit ())
      then (print_env r; print_newline ())
      else 
        let (l', r') = trans p (l, r) in
          exec (l', r')
  in
	  (try exec ((entry ()), (initerr ()))
		 with Error s -> print_string ("Fatal error:" ^ s ^ ".\n"))
  