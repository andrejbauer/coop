(** The typing context *)
type context

(** Type errors *)
type error

(** Exception signalling a type error. *)
exception Error of error Location.located

(** Print error description. *)
val print_error : error -> Format.formatter -> unit

(** The initial typing context *)
val initial : context

(** Type-check a top-level command. *)
val toplevel : quiet:bool -> context -> Dsyntax.toplevel -> context * Rsyntax.toplevel

(** Type-check the contents of a file. *)
val topfile : quiet:bool -> context -> Dsyntax.toplevel list -> context * Rsyntax.toplevel list
