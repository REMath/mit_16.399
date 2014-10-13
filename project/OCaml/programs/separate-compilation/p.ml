
module type A_signature =
  sig 
    type entier
    val i : unit -> entier
    val p : entier -> unit
  end;;

module B_implementation =
struct
  type entier = int
  let c = ref 1
  let i () = c := !c + !c ; !c
  let p v = print_int v; print_newline ()
end;;
module B = (B_implementation:A_signature);;

module type C_signature =
  functor (A: A_signature) ->
	 sig
		val i : unit -> A.entier
		val p : A.entier -> unit
	 end;;

module C_implementation =
  functor (A: A_signature) ->
  struct
	 type ent = A.entier
	 let f i = ()
	 let i () = f (A.i ()); A.i ()
	 let p = A.p
  end;;
  
module C = (C_implementation:C_signature);;

module D = C(B);;

D.p (D.i ());;

