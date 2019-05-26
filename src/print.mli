(** Pretty-printing support. *)

(** Print an error message *)
val error : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a

(** Print a warning *)
val warning : ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a

(** Print an expression, possibly parenthesized. *)
val print :
  ?at_level:Level.t -> ?max_level:Level.t ->
  Format.formatter -> ('a, Format.formatter, unit, unit, unit, unit) format6 -> 'a

(** Unicode or ASCII versions of the arrow, [->] or [→]. *)
val char_arrow : unit -> string

(** Unicode or ASCII versions of the double arrow, [=>] or [⇒]. *)
val char_darrow : unit -> string

(** Unicode or ASCII versions of the product [*] or [×]. *)
val char_times : unit -> string

(** Print a sequence. *)
val sequence :
  ('a -> Format.formatter -> unit) -> string -> 'a list -> Format.formatter -> unit
