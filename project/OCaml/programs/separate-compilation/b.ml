open A;;
module B_implementation =
struct
  type entier = int
  let c = ref 1
  let i () = c := !c + !c ; !c
  let p v = print_int v; print_newline ()
end;;
module B = (B_implementation:A_signature);;
