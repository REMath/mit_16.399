
module A = struct
module type A_signature =
  sig 
    type entier
    val i : unit -> entier
    val p : entier -> unit
  end;;
end;;
module B : sig
open A;;
module B : A_signature;;
end = struct
open A;;
module B_implementation =
struct
  type entier = int
  let c = ref 1
  let i () = c := !c + !c ; !c
  let p v = print_int v; print_newline ()
end;;
module B = (B_implementation:A_signature);;
end;;
module C : sig
open A;;
module type C_signature =
  functor (A: A_signature) ->
	 sig
		val i : unit -> A.entier
		val p : A.entier -> unit
	 end;;
module C : C_signature;;
end = struct
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
end;;
module D = struct
open B;;
open C;;
module D = C(B);;
end;;
open D;;
D.p (D.i ());;
