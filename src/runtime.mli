(** Runtime environment *)
type environment

(** Runtime error *)
type error

(** Runtime exception for raising an error *)
exception Error of error Location.located

(** Print a runtime error *)
val print_error : error -> Format.formatter -> unit

(** The initial runtime environment *)
val initial : environment

(** Evaluate a toplevel command *)
val eval_toplevel :
  quiet:bool -> environment -> Syntax.toplevel -> environment

(** Evaluate a list of toplevel commands *)
val eval_topfile :
  quiet:bool -> environment -> Syntax.toplevel list -> environment
