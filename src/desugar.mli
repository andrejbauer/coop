(** Desugaring errors *)
type desugar_error

(** A desugaring context is a list of known identifiers, which is used to
   compute de Bruijn indices. *)
type context

(** The initial empty context *)
val initial : context

(** The exception signalling a desugaring error*)
exception Error of desugar_error Location.located

(** Print desugaring error. *)
val print_error : desugar_error -> Format.formatter -> unit

(** Load a file and desugar it. *)
val load : context -> string -> context * Dsyntax.toplevel list

(** Desugar a toplevel. *)
val toplevel : context -> Sugared.toplevel -> context * Dsyntax.toplevel
