(** Runtime values *)

(** Runtime value *)
type t =
  | Abstract
  | Numeral of int
  | Boolean of bool
  | Quoted of string
  | Constructor of Name.t * t option
  | Tuple of t list
  | Closure of (t -> t result)
  | Runner of cooperation Name.Map.t
  | Shell of shell

and world = World of t

and 'a result =
  | Val of 'a
  | Operation of Name.t * t * (t -> 'a result)
  | Signal of Name.t * t

and cooperation = t * world -> (t * world) result

and shell = (t * world -> t * world) Name.Map.t * world

(** Give a descriptive name of a value. *)
val name : t -> string

(** Give a descriptive name of a value, in plural. *)
val names : t -> string

(** The empty shell *)
val pure_shell : shell

(** Pretty-print a value. *)
val print : ?max_level:Level.t -> t -> Format.formatter -> unit
