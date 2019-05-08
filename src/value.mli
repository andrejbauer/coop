type t =
  | Numeral of int
  | Boolean of bool
  | Tuple of t list
  | Closure of closure
  | Comodel of cooperation Name.Map.t

and world = t

and result =
  | Val of t
  | Operation of Name.t * t * closure

and closure = t -> result

and cooperation = t * world -> t * world

val print : ?max_level:Level.t -> t -> Format.formatter -> unit
