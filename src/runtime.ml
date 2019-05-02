type value =
  | Numeral of int
  | Tuple of value list
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
  | PatternMismatch

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

  | PatternMismatch ->
     Format.fprintf ppf "pattern mismatch"

let initial = []

let extend v env = v :: env

let lookup ~loc i env =
  try
    List.nth env i
  with
  | Failure _ -> error ~loc (InvalidDeBruijn i)

let rec match_pattern env p v =
  match p, v with

  | Rsyntax.PattAnonymous, _ -> Some env

  | Rsyntax.PattVar, _ ->
     let env = extend v env in
     Some env

  | Rsyntax.PattNumeral m, Numeral n ->
     if m = n then Some env else None

  | Rsyntax.PattTuple ps, Tuple vs ->
     match_pattern_tuple env ps vs

  | _, Closure _ -> None

  | (Rsyntax.PattTuple _, Numeral _ |
     Rsyntax.PattNumeral _, Tuple _) ->
     None

and match_pattern_tuple env ps vs =
  let rec fold env ps vs =
    match ps, vs with
    | [], [] -> Some env
    | p :: ps, v :: vs ->
       begin
         match match_pattern env p v with
         | None -> None
         | Some env -> fold env ps vs
       end
    | [], _::_ | _::_, [] -> None
  in
  fold env ps vs

let match_clauses ~loc env ps v =
  let rec fold = function
    | [] -> error ~loc PatternMismatch
    | (p, c) :: lst ->
       begin
         match match_pattern env p v with
         | None -> fold lst
         | Some env -> (env, c)
       end
  in
  fold ps

let generic op =
  Closure (fun u -> Operation (op, u, (fun u -> Return u)))

let rec print_value ?max_level v ppf =
  match v with

  | Numeral k -> Format.fprintf ppf "%d" k

  | Tuple lst ->
     Format.fprintf ppf "(%t)"
       (Print.sequence (print_value ~max_level:Level.tuple_arg) "," lst)

  | Closure _ -> Format.fprintf ppf "<fun>"

let as_value ~loc = function

  | Return v -> v

  | Operation (op, _, _) ->
     error ~loc (UnhandledOperation op)

let as_closure ~loc = function

  | Closure f -> f

  | Numeral _ | Tuple _ ->
     error ~loc FunctionExpected
