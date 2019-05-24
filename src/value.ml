type t =
  | Abstract
  | Numeral of int
  | Boolean of bool
  | Quoted of string
  | Constructor of Name.t * t option
  | Tuple of t list
  | Closure of (t -> t result)
  | Comodel of t * cooperation Name.Map.t

and world = t

and 'a result =
  | Val of 'a
  | Operation of Name.t * t * (t -> 'a result)
  | Signal of Name.t * t

and cooperation = t * world -> (t * world) result

let rec print ?max_level v ppf =
  match v with
  | Abstract -> Format.fprintf ppf "<abstr>"

  | Numeral k -> Format.fprintf ppf "%d" k

  | Boolean b -> Format.fprintf ppf "%b" b

  | Quoted s -> Format.fprintf ppf "\"%s\"" (String.escaped s)

  | Constructor (Name.Ident (_, Name.Prefix) as x, Some (Tuple [e])) ->
     Print.print ?max_level ~at_level:Level.prefix ppf "%t@ %t"
       (Name.print ~parentheses:false x)
       (print ~max_level:Level.prefix_arg e)

  | Constructor (Name.Ident (_, Name.Infix fixity) as x, Some (Tuple [e1; e2])) ->
     let lvl, lvl_left, lvl_right = Level.infix fixity in
     Print.print ?max_level ~at_level:lvl ppf "%t@ %t@ %t"
       (print ~max_level:lvl_left e1)
       (Name.print ~parentheses:false x)
       (print ~max_level:lvl_right e2)

  | Constructor (x, Some v) ->
     Print.print ?max_level ~at_level:Level.constr ppf "%t@ %t"
       (Name.print ~parentheses:true x)
       (print ~max_level:Level.constr_arg v)

  | Constructor (x, None) ->
     Name.print ~parentheses:true x ppf

  | Tuple lst ->
     Format.fprintf ppf "(%t)" (print_tuple lst)

  | Closure _ -> Format.fprintf ppf "<fun>"

  | Comodel _ -> Format.fprintf ppf "<comodel>"

and print_tuple lst ppf =
  Print.sequence (print ~max_level:Level.tuple_arg) "," lst ppf
