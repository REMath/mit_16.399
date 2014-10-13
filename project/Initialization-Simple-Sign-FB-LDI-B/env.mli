(* env.mli *)
open Abstract_Syntax
open Variables
open Values
type env                                               (* environments     *)
val initerr   : unit -> env                            (* uninitialization *)
val copy      : env -> env                             (* copy             *)
val get       : env -> variable -> machine_int         (* r(X)             *)
val set       : env -> variable -> machine_int -> unit (* r[X <- v]        *)
val print_env : env -> unit                            (* printing         *)    