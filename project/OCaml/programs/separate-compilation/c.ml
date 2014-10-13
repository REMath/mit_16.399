open A;;
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
module C = (C_implementation:C_signature)
