(** Kinds of names. *)
type fixity =
  | Word (** an ordinary word *)
  | Anonymous of int (** an anonymous name _ *)
  | Prefix (** prefix operator *)
  | Infix of Level.infix (** infix operator *)

(** An identifier. *)
type t = Ident of string * fixity

(** Are the given identifiers equal? *)
val equal : t -> t -> bool

(** Print an identifier. *)
val print : ?parentheses:bool -> t -> Format.formatter -> unit

(** Create a fresh anonymous name. *)
val anonymous : unit -> t
