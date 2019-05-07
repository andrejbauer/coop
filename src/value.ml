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

let rec print ?max_level v ppf =
  match v with

  | Numeral k -> Format.fprintf ppf "%d" k

  | Tuple lst ->
     Format.fprintf ppf "(%t)"
       (Print.sequence (print ~max_level:Level.tuple_arg) "," lst)

  | Closure _ -> Format.fprintf ppf "<fun>"

  | Comodel _ -> Format.fprintf ppf "<comodel>"
