(* labels.mli *)
open Abstract_Syntax
(* labels *)
val at               : com   -> label       (* command entry label *)
val after            : com   -> label       (* command exit label  *)
val incom            : label -> com -> bool (* label in command    *)
val number_of_labels : unit  -> int
val entry            : unit  -> label       (* program entry label *)
val exit             : unit  -> label       (* program exit label  *)
val print_label      : label -> unit
val string_of_label  : label -> string
