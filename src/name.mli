(** Kinds of names. *)
type fixity =
  | Word (** an ordinary word *)
  | Anonymous of int (** an anonymous name _ *)
  | Prefix (** prefix operator *)
  | Infix of Level.infix (** infix operator *)

(** An identifier. *)
type t = Ident of string * fixity

(** A set of identifiers. *)
module Set :
sig
  type elt = t
  type t

  val empty : t
  val is_empty : t -> bool
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val elements : t -> elt list
  val subset : t -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
end

(** A map from identifiers. *)
module Map :
sig
  type key = t
  type 'a t
  val empty : 'a t
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
end

(** Are the given identifiers equal? *)
val equal : t -> t -> bool

(** Print an identifier. *)
val print : ?parentheses:bool -> t -> Format.formatter -> unit

(** Create a fresh anonymous name. *)
val anonymous : unit -> t
