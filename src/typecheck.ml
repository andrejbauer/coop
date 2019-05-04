(** Terminus type checking. *)

(** Typing context *)
type context = (Name.t * Syntax.expr_ty) list

(** Initial typing context *)
let initial = []

(** Type errors *)
type error =
  | InvalidName of Name.t
  | PattTypeMismatch of Syntax.expr_ty
  | ExprTypeMismatch of Syntax.expr_ty * Syntax.expr_ty
  | CompTypeMismatch of Syntax.comp_ty * Syntax.comp_ty
  | TypeExpectedButFunction of Syntax.expr_ty
  | TypeExpectedButTuple of Syntax.expr_ty
  | TupleTooShort of Syntax.expr_ty
  | TupleTooLong of Syntax.expr_ty
  | FunctionExpected of Syntax.expr_ty
  | CannotInferArgument of Name.t
  | CannotInferMatch

exception Error of error Location.located

(** [error ~loc err] raises the given type-checking error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

let print_error err ppf =
  match err with

  | InvalidName x -> Format.fprintf ppf "invalid name %t, please report" (Name.print x)

  | PattTypeMismatch ty_expected ->
     Format.fprintf ppf "this pattern should have type@ %t"
                        (Syntax.print_expr_ty ty_expected)

  | ExprTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type@ %t but has type@ %t"
                        (Syntax.print_expr_ty ty_expected)
                        (Syntax.print_expr_ty ty_actual)

  | CompTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type@ %t but has type@ %t"
                        (Syntax.print_comp_ty ty_expected)
                        (Syntax.print_comp_ty ty_actual)

  | TypeExpectedButFunction ty ->
     Format.fprintf ppf "this expression is a function but should have type@ %t"
                        (Syntax.print_expr_ty ty)

  | TypeExpectedButTuple ty ->
     Format.fprintf ppf "this expression is a tuple but should have type@ %t"
                        (Syntax.print_expr_ty ty)

  | TupleTooShort ty ->
     Format.fprintf ppf "this tuple has too few components, it should have type@ %t"
                        (Syntax.print_expr_ty ty)

  | TupleTooLong ty ->
     Format.fprintf ppf "this tuple has too many components, it should have type@ %t"
                        (Syntax.print_expr_ty ty)

  | FunctionExpected ty ->
     Format.fprintf ppf "this expression should be a function but has type@ %t"
                        (Syntax.print_expr_ty ty)

  | CannotInferArgument x ->
     Format.fprintf ppf "cannot infer the type of@ %t" (Name.print x)

  | CannotInferMatch ->
     Format.fprintf ppf "cannot infer the type of this match statement"


(** Extend the context with the type of deBruijn index 0 *)
let extend x ty ctx = (x, ty) :: ctx

let rec extends xts ctx =
  match xts with
  | [] -> ctx
  | (x, t) :: xts -> extends xts (extend x t ctx)

(** Lookup the index and the type of a name *)
let lookup ~loc x ctx =
  let rec fold k = function
    | [] -> error ~loc (InvalidName x)
    | (y,t) :: ctx when Name.equal x y -> (k, t)
    | _ :: ctx -> fold (k+1) ctx
  in
  fold 0 ctx

(** Check that a type is valid. Retrn the processed type. *)
let rec expr_ty = function

  | Desugared.Int -> Syntax.Int

  | Desugared.Product lst ->
     let lst = List.map expr_ty lst in
     Syntax.Product lst

  | Desugared.Arrow (t1, t2) ->
     let t1 = expr_ty t1
     and t2 = comp_ty t2 in
     Syntax.Arrow (t1, t2)

and comp_ty ty = Syntax.purely (expr_ty ty)

(** Typecheck a pattern, return processed pattern and the list of identifiers
   and types bound by the pattern *)
let check_pattern patt ty =
  let rec fold xts {Location.data=p'; loc} t =
    match p', t with

    | Desugared.PattAnonymous, _ ->
       Syntax.PattAnonymous, xts

    | Desugared.PattVar x, _ ->
       Syntax.PattVar, (x, t) :: xts

    | Desugared.PattNumeral n, Syntax.Int ->
       Syntax.PattNumeral n, xts

    | Desugared.PattTuple ps, Syntax.Product ts ->
       fold_tuple ~loc xts [] ps ts

    | ((Desugared.PattTuple _, (Syntax.Int | Syntax.Arrow _)) |
         (Desugared.PattNumeral _, (Syntax.Product _ | Syntax.Arrow _))) ->
       error ~loc (PattTypeMismatch ty)

  and fold_tuple ~loc xts ps' ps ts =
    match ps, ts with
    | [], [] ->
       let ps' = List.rev ps' in
       Syntax.PattTuple ps', xts
    | p :: ps, t :: ts ->
       let p', xts = fold xts p t in
       fold_tuple ~loc xts (p' :: ps') ps ts
    | ([], _::_ | _::_, []) ->
       error ~loc (PattTypeMismatch (Syntax.Product ts))
  in

  let p, xts = fold [] patt ty in
  p, List.rev xts

(** Extend the context by typechecking the pattern against the type *)
let extend_pattern ctx p t =
  let p, xts = check_pattern p t in
  let ctx = extends xts ctx in
  ctx, p

(** Extend the context by typechecking the pattern against the type,
    also return the list of bound names with their types. *)
let top_extend_pattern ctx p t =
  let p, xts = check_pattern p t in
  let ctx = extends xts ctx in
  ctx, p, xts

(** [infer_expr ctx e] infers the expression type [ty] of an expression [e]. It
   returns the processed expression [e] and its type [ty]. *)
let rec infer_expr (ctx : context) {Location.data=e'; loc} =
  let locate = Location.locate ~loc in
  match e' with
  | Desugared.Var x ->
     let k, ty = lookup ~loc x ctx in
     locate (Syntax.Var k), ty

  | Desugared.Numeral n ->
     locate (Syntax.Numeral n), Syntax.Int

  | Desugared.Tuple lst ->
     let lst = List.map (infer_expr ctx) lst in
     locate (Syntax.Tuple (List.map fst lst)),  Syntax.Product (List.map snd lst)

  | Desugared.Lambda (x, Some t, c) ->
     let t = expr_ty t in
     let ctx = extend x t ctx in
     let c, c_ty = infer_comp ctx c in
     locate (Syntax.Lambda c), Syntax.Arrow (t, c_ty)

  | Desugared.Lambda (x, None, _) ->
     error ~loc (CannotInferArgument x)

  | Desugared.AscribeExpr (e, t) ->
     let t = expr_ty t in
     let e = check_expr ctx e t in
     e, t

(** [infer_comp ctx c] infers the type [ty] of a computation [c]. It returns
    the processed computation [c] and its type [ty].  *)
and infer_comp (ctx : context) {Location.data=c'; loc} =
  let locate = Location.locate ~loc in
  match c' with

  | Desugared.Return e ->
     let e, e_ty = infer_expr ctx e in
     locate (Syntax.Return e), Syntax.purely e_ty

  | Desugared.Let (p, c1, c2) ->
     let c1, c1_ty = infer_comp ctx c1 in
     let ctx, p = extend_pattern ctx p (Syntax.purify c1_ty) in
     let c2, c2_ty = infer_comp ctx c2 in
     locate (Syntax.Let (p, c1, c2)), c2_ty

  | Desugared.Match (e, lst) ->
     let e, e_ty = infer_expr ctx e in
     let lst, ty = infer_match_clauses ~loc ctx e_ty lst in
     locate (Syntax.Match (e, lst)), ty

  | Desugared.Apply (e1, e2) ->
     let e1, t1 = infer_expr ctx e1 in
     begin
       match t1 with
       | Syntax.Arrow (u1, u2) ->
          let e2 = check_expr ctx e2 u1 in
          locate (Syntax.Apply (e1, e2)), u2

       | (Syntax.Int | Syntax.Product _) ->
          error ~loc:(e1.Location.loc) (FunctionExpected t1)
     end

  | Desugared.AscribeComp (c, t) ->
     let t = comp_ty t in
     let c = check_comp ctx c t in
     c, t

and infer_match_clauses ~loc ctx patt_ty lst =
  match lst with
  | [] -> error ~loc CannotInferMatch
  | (p, c) :: lst ->
     let ctx, p = extend_pattern ctx p patt_ty in
     let c, c_ty = infer_comp ctx c in
     let lst = check_match_clauses ctx patt_ty c_ty lst in
     ((p, c) :: lst), c_ty

(** [check_expr ctx e ty] checks that expression [e] has type [ty] in context [ctx].
    It returns the processed expression [e]. *)
and check_expr (ctx : context) ({Location.data=e'; loc} as e) ty =
  let locate = Location.locate ~loc in
  match e' with

  | Desugared.Lambda (x, None, e) ->
     begin
       match ty with
       | Syntax.Arrow (t, u) ->
          let c = check_comp (extend x t ctx) e u in
          locate (Syntax.Lambda c)
       | (Syntax.Int | Syntax.Product _) ->
          error ~loc (TypeExpectedButFunction ty)
     end

  | Desugared.Tuple es ->
     begin
       match ty with
       | Syntax.Product ts ->
          let rec fold es ts es' =
            match es, ts with
            | [], [] ->
               let es' = List.rev es' in
               locate (Syntax.Tuple es')
            | e :: es, t :: ts ->
               let e' = check_expr ctx e t in
               fold es ts (e' :: es')
            | _ :: _, [] -> error ~loc (TupleTooLong ty)
            | [], _ :: _ -> error ~loc (TupleTooShort ty)
          in
          fold es ts []

       | (Syntax.Int | Syntax.Arrow _) ->
          error ~loc (TypeExpectedButTuple ty)
     end

  | (Desugared.Numeral _ | Desugared.Lambda (_, Some _, _) | Desugared.Var _ | Desugared.AscribeExpr _) ->
     let e, ty' = infer_expr ctx e in
     if Syntax.equal_expr_ty ty ty'
     then
       e
     else
       error ~loc (ExprTypeMismatch (ty, ty'))

(** [check_comp ctx c ty] checks that computation [c] has type [ty] in context [ctx].
    It returns the processed computation [c]. *)
and check_comp ctx ({Location.data=c'; loc} as c) ty =
  let locate = Location.locate ~loc in
  match c' with

  | Desugared.Return e ->
    let (Syntax.CompTy (ty, _)) = ty in
    let e = check_expr ctx e ty in
    locate (Syntax.Return e)

  | Desugared.Match (e, lst) ->
     let e, e_ty = infer_expr ctx e in
     let lst = check_match_clauses ctx e_ty ty lst in
     locate (Syntax.Match (e, lst))

  | Desugared.Let (p, c1, c2) ->
     let c1, t1 = infer_comp ctx c1 in
     let ctx, p = extend_pattern ctx p (Syntax.purify t1) in
     let c2 = check_comp ctx c2 ty in
     locate (Syntax.Let (p, c1, c2))

  | (Desugared.Apply _ | Desugared.AscribeComp _) ->
     let c, ty' = infer_comp ctx c in
     if Syntax.equal_comp_ty ty ty'
     then
       c
     else
       error ~loc (CompTypeMismatch (ty, ty'))

and check_match_clauses ctx patt_ty ty lst =
  List.map (check_match_clause ctx patt_ty ty) lst

and check_match_clause ctx patt_ty ty (p, c) =
  let ctx, p = extend_pattern ctx p patt_ty in
  let c = check_comp ctx c ty in
  (p, c)

and toplevel ~quiet ctx {Location.data=d'; loc} =
  let ctx, d' =
    match d' with

    | Desugared.TopLoad lst ->
       let ctx, lst = topfile ~quiet ctx lst in
       ctx, Syntax.TopLoad lst

    | Desugared.TopLet (p, c) ->
       let c, ty = infer_comp ctx c in
       let ctx, p, xts = top_extend_pattern ctx p (Syntax.purify ty) in
       ctx, Syntax.TopLet (p, xts, c)

    | Desugared.TopComp c ->
       let c, ty = infer_comp ctx c in
       ctx, Syntax.TopComp (c, ty)

    | Desugared.DeclOperation (op, ty1, ty2) ->
       let ty1 = expr_ty ty1
       and ty2 = Syntax.pollute (Syntax.purely (expr_ty ty2)) op in
       let ctx = extend op (Syntax.Arrow (ty1, ty2)) ctx in
       ctx, Syntax.DeclOperation (op, ty1, ty2)

  in
  ctx, Location.locate ~loc d'

and topfile ~quiet (ctx : context) lst =
  let rec fold ctx ds = function
    | [] -> ctx, List.rev ds
    | top_cmd :: lst ->
       let ctx, d = toplevel ~quiet ctx top_cmd in
       fold ctx (d :: ds) lst
  in
  fold ctx [] lst
