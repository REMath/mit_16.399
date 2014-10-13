open A;;
module type C_signature =
  functor (A: A_signature) ->
	 sig
		val i : unit -> A.entier
		val p : A.entier -> unit
	 end;;
module C : C_signature;;
