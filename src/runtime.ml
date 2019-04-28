type value =
  | Numeral of int
  | Closure of closure

and result =
  | Return of value
  | Operation of Name.ident * value * closure

and closure = value -> result

type environment = value list

type error =
  | InvalidDeBruijn of int
  | UnhandledOperation of Name.ident
  | FunctionExpected

exception Error of error Location.located

let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

let print_error err ppf =
  match err with

  | InvalidDeBruijn i ->
     Format.fprintf ppf "invalid de Bruijn index %d, please report" i

  | UnhandledOperation op ->
     Format.fprintf ppf "unhandled operation %t" (Name.print_ident op)

  | FunctionExpected ->
     Format.fprintf ppf "function expected, please report"

let initial = []

let extend v env = v :: env

let lookup ~loc i env =
  try
    List.nth env i
  with
  | Failure _ -> error ~loc (InvalidDeBruijn i)

let generic op =
  Closure (fun u -> Operation (op, u, (fun u -> Return u)))

let print_value ?max_level v ppf =
  match v with

  | Numeral k -> Format.fprintf ppf "%d" k

  | Closure _ -> Format.fprintf ppf "<fun>"

let as_value ~loc = function

  | Return v -> v

  | Operation (op, _, _) ->
     error ~loc (UnhandledOperation op)

let as_closure ~loc = function

  | Closure f -> f

  | Numeral _ ->
     error ~loc FunctionExpected
