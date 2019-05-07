type t =
  | Numeral of int
  | Tuple of t list
  | Closure of closure
  | Comodel of world * cooperation Name.Map.t

and world = World of t

and result =
  | Val of t
  | Operation of Name.t * t * closure

and closure = t -> result

and cooperation = t * world -> t * world

val print : ?max_level:Level.t -> t -> Format.formatter -> unit
