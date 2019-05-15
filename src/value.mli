type t =
  | Numeral of int
  | Boolean of bool
  | Constructor of Name.t * t option
  | Tuple of t list
  | Closure of (t -> t result)
  | Comodel of cooperation Name.Map.t

and world = t

and 'a result =
  | Val of 'a
  | Operation of Name.t * t * (t -> 'a result)
  | Signal of Name.t * t

and cooperation = t * world -> (t * world) result

val print : ?max_level:Level.t -> t -> Format.formatter -> unit
