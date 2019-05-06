type value =
  | Numeral of int
  | Tuple of value list
  | Closure of closure
  | Comodel of world * cooperation Name.Map.t

and world = World of value

and result =
  | Return of value
  | Operation of Name.t * value * closure

and closure = value -> result

and cooperation = value * world -> value * world

type environment = value list

type error =
  | InvalidDeBruijn of int
  | UnhandledOperation of Name.t
  | FunctionExpected
  | ComodelExpected
  | PairExpected
  | PatternMismatch

exception Error of error Location.located

let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

let print_error err ppf =
  match err with

  | InvalidDeBruijn i ->
     Format.fprintf ppf "invalid de Bruijn index %d, please report" i

  | UnhandledOperation op ->
     Format.fprintf ppf "unhandled operation %t" (Name.print op)

  | FunctionExpected ->
     Format.fprintf ppf "function expected, please report"

  | ComodelExpected ->
     Format.fprintf ppf "comodel expected, please report"

  | PairExpected ->
     Format.fprintf ppf "pair expected, please report"

  | PatternMismatch ->
     Format.fprintf ppf "pattern mismatch"

let initial = []

let extend v env = v :: env

let rec extends vs env =
  match vs with
  | [] -> env
  | v :: vs ->
     let env = extend v env in
     extends vs env

let lookup ~loc i env =
  try
    List.nth env i
  with
  | Failure _ -> error ~loc (InvalidDeBruijn i)

let match_pattern p v =
  let rec fold us p v =
    match p, v with

    | Syntax.PattAnonymous, _ -> Some us

    | Syntax.PattVar, _ ->
       Some (v :: us)

  | Syntax.PattNumeral m, Numeral n ->
     if m = n then Some us else None

  | Syntax.PattTuple ps, Tuple vs ->
     fold_tuple us ps vs

  | _, Closure _ -> None

  | _, Comodel _ -> None

  | (Syntax.PattTuple _, Numeral _ |
     Syntax.PattNumeral _, Tuple _) ->
     None

  and fold_tuple us ps vs =
    match ps, vs with
    | [], [] -> Some us
    | p :: ps, v :: vs ->
       begin
         match fold us p v with
         | None -> None
         | Some us -> fold_tuple us ps vs
       end
    | [], _::_ | _::_, [] -> None
  in

  match fold [] p v with
  | None -> None
  | Some us -> Some (List.rev us)


let extend_pattern ~loc p v env =
  match match_pattern p v with
  | None -> error ~loc PatternMismatch
  | Some us -> extends us env

let top_extend_pattern ~loc p v env =
  match match_pattern p v with
  | None -> error ~loc PatternMismatch
  | Some us -> extends us env, us

let match_clauses ~loc env ps v =
  let rec fold = function
    | [] -> error ~loc PatternMismatch
    | (p, c) :: lst ->
       begin
         match match_pattern p v with
         | None -> fold lst
         | Some us -> (extends us env, c)
       end
  in
  fold ps

let rec print_value ?max_level v ppf =
  match v with

  | Numeral k -> Format.fprintf ppf "%d" k

  | Tuple lst ->
     Format.fprintf ppf "(%t)"
       (Print.sequence (print_value ~max_level:Level.tuple_arg) "," lst)

  | Closure _ -> Format.fprintf ppf "<fun>"

  | Comodel _ -> Format.fprintf ppf "<comodel>"

let as_pair ~loc = function
  | Tuple [v1; v2] -> (v1, v2)
  | Closure _ | Numeral _ | Tuple ([] | [_] | _::_::_::_) | Comodel _ ->
     error ~loc PairExpected

let as_closure ~loc = function
  | Closure f -> f
  | Numeral _ | Tuple _ | Comodel _ -> error ~loc FunctionExpected

let as_value ~loc = function
  | Return v -> v
  | Operation (op, _, _) -> error ~loc (UnhandledOperation op)

let as_comodel ~loc = function
  | Comodel (w, cmdl) -> (w, cmdl)
  | Numeral _ | Tuple _ | Closure _ -> error ~loc ComodelExpected

(** The result monad *)
let rec bind r k =
  match r with
  | Return v -> k v
  | Operation (op, u, l) -> Operation (op, u, fun x -> let r = l x in bind r k)

(*** Evaluation ***)

let rec eval_expr env {Location.data=e'; loc} =
  match e' with

  | Syntax.Numeral k -> Numeral k

  | Syntax.Var i -> lookup ~loc i env

  | Syntax.Tuple lst ->
     let lst = List.map (eval_expr env) lst in
     Tuple lst

  | Syntax.Lambda c ->
     let f v =
       let env = extend v env in
       eval_comp env c
     in
     Closure f

  | Syntax.Comodel (e, lst) ->
     let w = eval_expr env e in
     let coop px pw c =
       let loc = c.Location.loc in
       fun (u, World w) ->
       let env = extend_pattern ~loc px u env in
       let env = extend_pattern ~loc pw w env in
       let r = eval_comp env c in
       let (v, w) = as_pair ~loc (as_value ~loc r) in
       (v, World w)
     in
     let cmdl =
       List.fold_left
         (fun cmdl (op, px, pw, c) -> Name.Map.add op (coop px pw c) cmdl)
         Name.Map.empty
         lst
     in
     Comodel (World w, cmdl)

and eval_comp env {Location.data=c'; loc} =
  match c' with

  | Syntax.Return e ->
     let v = eval_expr env e in
     Return v

  | Syntax.Match (e, lst) ->
     let v = eval_expr env e in
     let env, c = match_clauses ~loc env lst v in
     eval_comp env c

  | Syntax.Apply (e1, e2) ->
     let v1 = eval_expr env e1 in
     let f = as_closure ~loc v1 in
     let v2 = eval_expr env e2 in
     f v2

  | Syntax.Let (p, c1, c2) ->
     let r = eval_comp env c1 in
     let k v = eval_comp (extend_pattern ~loc p v env) c2 in
     bind r k

  | Syntax.Operation (op, u) ->
     let u = eval_expr env u in
     Operation (op, u, (fun v -> Return v))

  | Syntax.Using (cmdl, c, fin) ->
     let (w, cmdl) = as_comodel ~loc (eval_expr env cmdl)
     and r = eval_comp env c
     and fin = eval_finally ~loc env fin in
     using ~loc env cmdl w r fin

and eval_finally ~loc env (px, pw, c) =
  fun (v, World w) ->
  let env = extend_pattern ~loc px v env in
  let env = extend_pattern ~loc pw w env in
  eval_comp env c

and using ~loc env cmdl w r fin =
  let rec tensor w r =
    match r with

    | Return v -> fin (v, w)

    | Operation (op, u, k) ->
       begin
         match Name.Map.find op cmdl with
         | None -> error ~loc (UnhandledOperation op)
         | Some coop ->
            let (v, w') = coop (u, w) in
            tensor w' (k v)
       end
  in
  tensor w r

let rec eval_toplevel ~quiet env {Location.data=d'; loc} =
  match d' with

  | Syntax.TopLoad cs ->
     eval_topfile ~quiet env cs

  | Syntax.TopLet (p, xts, c) ->
     let r = eval_comp env c in
     let v = as_value ~loc r in
     let env, vs = top_extend_pattern ~loc p v env in
     if not quiet then
       List.iter2
         (fun (x, ty) v ->
           Format.printf "@[<hov>val %t@ :@ %t@ =@ %t@]@."
             (Name.print x)
             (Syntax.print_expr_ty ty)
             (print_value v))
         xts vs ;
     env

  | Syntax.TopComp (c, ty) ->
     let r = eval_comp env c in
     let v = as_value ~loc r in
     if not quiet then
       Format.printf "@[<hov>- :@ %t@ =@ %t@]@."
         (Syntax.print_expr_ty ty)
         (print_value v) ;
     env

  | Syntax.DeclOperation (op, ty1, ty2) ->
     if not quiet then
       Format.printf "@[<hov>operation %t@ :@ %t@ %s@ %t@."
         (Name.print op)
         (Syntax.print_expr_ty ty1)
         (Print.char_arrow ())
         (Syntax.print_expr_ty ty2) ;
     env

and eval_topfile ~quiet env lst =
  let rec fold env = function
    | [] -> env
    | top_cmd :: lst ->
       let env = eval_toplevel ~quiet env top_cmd in
       fold env lst
  in
  fold env lst
