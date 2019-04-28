(** Evaluate a toplevel command *)
val toplevel :
  quiet:bool -> Runtime.environment -> Rsyntax.toplevel -> Runtime.environment

(** Evaluate a list of toplevel commands *)
val topfile :
  quiet:bool -> Runtime.environment -> Rsyntax.toplevel list -> Runtime.environment
