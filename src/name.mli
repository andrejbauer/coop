(** Handling of identifiers and variable names. *)

(** Kinds of identifiers. *)
type fixity =
  | Word (** an ordinary word *)
  | Anonymous of int (** an anonymous name _ *)
  | Prefix (** prefix operator *)
  | Infix of Level.infix (** infix operator *)

(** An identifier. *)
type t = Ident of string * fixity

(** An operation name. *)
type op = Op of t

(** A set of identifiers. *)
module Idset :
sig
  (** These are just an interface to the [Map] module from the standard library. *)

  type elt = t
  type t

  val empty : t
  val is_empty : t -> bool
  val remove : elt -> t -> t
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val elements : t -> elt list
  val subset : t -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

(** A set of operation names. *)
module Opset :
sig
  (** These are just an interface to the [Map] module from the standard library. *)

  type elt = op
  type t

  val empty : t
  val is_empty : t -> bool
  val remove : elt -> t -> t
  val add : elt -> t -> t
  val mem : elt -> t -> bool
  val elements : t -> elt list
  val subset : t -> t -> bool
  val union : t -> t -> t
  val inter : t -> t -> t
  val diff : t -> t -> t
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
end

(** A map from identifiers. *)
module Map :
sig
  (** These are just an interface to the [Map] module from the standard library. *)

  type key = t
  type 'a t
  val empty : 'a t
  val mem : key -> 'a t -> bool
  val add : key -> 'a -> 'a t -> 'a t
  val find : key -> 'a t -> 'a option
  val merge : (key -> 'a option -> 'a option -> 'a option) -> 'a t -> 'a t -> 'a t
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val map : ('a -> 'b) -> 'a t -> 'b t
end

(** Are the given identifiers equal? *)
val equal : t -> t -> bool

(** Print an identifier. *)
val print : ?parentheses:bool -> t -> Format.formatter -> unit

(** Create a fresh anonymous name. *)
val anonymous : unit -> t
