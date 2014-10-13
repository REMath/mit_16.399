module type A_signature =
  sig 
    type entier
    val i : unit -> entier
    val p : entier -> unit
  end;;
