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
  | Signal of Name.t * t

and closure = t -> result

and cooperation = t * world -> t * world

let rec print ?max_level v ppf =
  match v with

  | Numeral k -> Format.fprintf ppf "%d" k

  | Boolean b -> Format.fprintf ppf "%b" b

  | Tuple lst ->
     Format.fprintf ppf "(%t)"
       (Print.sequence (print ~max_level:Level.tuple_arg) "," lst)

  | Closure _ -> Format.fprintf ppf "<fun>"

  | Comodel _ -> Format.fprintf ppf "<comodel>"
