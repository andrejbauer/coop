(** Terminus type checking. *)

(** Typing context *)
type context = (Name.ident * Rsyntax.expr_ty) list

(** Initial typing context *)
let initial = []

(** Type errors *)
type error =
  | InvalidName of Name.ident
  | ExprTypeMismatch of Rsyntax.expr_ty * Rsyntax.expr_ty
  | CompTypeMismatch of Rsyntax.comp_ty * Rsyntax.comp_ty
  | TypeExpectedButFunction of Rsyntax.expr_ty
  | FunctionExpected of Rsyntax.expr_ty
  | CannotInferArgument of Name.ident

exception Error of error Location.located

(** [error ~loc err] raises the given type-checking error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

let print_error err ppf =
  match err with

  | InvalidName x -> Format.fprintf ppf "invalid name %t, please report" (Name.print_ident x)

  | ExprTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type@ %t but has type@ %t"
                        (Rsyntax.print_expr_ty ty_expected)
                        (Rsyntax.print_expr_ty ty_actual)

  | CompTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type@ %t but has type@ %t"
                        (Rsyntax.print_comp_ty ty_expected)
                        (Rsyntax.print_comp_ty ty_actual)

  | TypeExpectedButFunction ty ->
     Format.fprintf ppf "this expression is a function but should have type@ %t"
                        (Rsyntax.print_expr_ty ty)

  | FunctionExpected ty ->
     Format.fprintf ppf "this expression should be a function but has type@ %t"
                        (Rsyntax.print_expr_ty ty)

  | CannotInferArgument x ->
     Format.fprintf ppf "cannot infer the type of@ %t" (Name.print_ident x)

(** Extend the context with the type of deBruijn index 0 *)
let extend x ty ctx = (x, ty) :: ctx

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

  | Dsyntax.Int -> Rsyntax.Int

  | Dsyntax.Arrow (t1, t2) ->
     let t1 = expr_ty t1
     and t2 = comp_ty t2 in
     Rsyntax.Arrow (t1, t2)

and comp_ty ty = Rsyntax.purely (expr_ty ty)

(** [infer_expr ctx e] infers the expression type [ty] of an expression [e]. It
   returns the processed expression [e] and its type [ty]. *)
let rec infer_expr (ctx : context) {Location.data=e'; loc} =
  let locate = Location.locate ~loc in
  match e' with
  | Dsyntax.Var x ->
     let k, ty = lookup ~loc x ctx in
     locate (Rsyntax.Var k), ty

  | Dsyntax.Numeral n ->
     locate (Rsyntax.Numeral n), Rsyntax.Int

  | Dsyntax.Lambda (x, Some t, c) ->
     let t = expr_ty t in
     let ctx = extend x t ctx in
     let c, c_ty = infer_comp ctx c in
     locate (Rsyntax.Lambda c), Rsyntax.Arrow (t, c_ty)

  | Dsyntax.Lambda (x, None, _) ->
     error ~loc (CannotInferArgument x)

  | Dsyntax.AscribeExpr (e, t) ->
     let t = expr_ty t in
     let e = check_expr ctx e t in
     e, t

(** [infer_comp ctx c] infers the type [ty] of a computation [c]. It returns
    the processed computation [c] and its type [ty].  *)
and infer_comp (ctx : context) {Location.data=c'; loc} =
  let locate = Location.locate ~loc in
  match c' with

  | Dsyntax.Return e ->
     let e, e_ty = infer_expr ctx e in
     locate (Rsyntax.Return e), Rsyntax.purely e_ty

  | Dsyntax.Sequence (x, c1, c2) ->
     let c1, c1_ty = infer_comp ctx c1 in
     let ctx = extend x (Rsyntax.purify c1_ty) ctx in
     let c2, c2_ty = infer_comp ctx c2 in
     locate (Rsyntax.Sequence (c1, c2)), c2_ty

  | Dsyntax.Apply (e1, e2) ->
     let e1, t1 = infer_expr ctx e1 in
     begin
       match t1 with
       | Rsyntax.Arrow (u1, u2) ->
          let e2 = check_expr ctx e2 u1 in
          locate (Rsyntax.Apply (e1, e2)), u2

       | Rsyntax.Int -> error ~loc:(e1.Location.loc) (FunctionExpected t1)
     end

  | Dsyntax.AscribeComp (c, t) ->
     let t = comp_ty t in
     let c = check_comp ctx c t in
     c, t

(** [check_expr ctx e ty] checks that expression [e] has type [ty] in context [ctx].
    It returns the processed expression [e]. *)
and check_expr (ctx : context) ({Location.data=e'; loc} as e) ty =
  let locate = Location.locate ~loc in
  match e' with

  | Dsyntax.Lambda (x, None, e) ->
     begin
       match ty with
       | Rsyntax.Arrow (t, u) ->
          let c = check_comp (extend x t ctx) e u in
          locate (Rsyntax.Lambda c)
       | Rsyntax.Int -> error ~loc (TypeExpectedButFunction ty)
     end

  | (Dsyntax.Numeral _ | Dsyntax.Lambda (_, Some _, _) | Dsyntax.Var _ | Dsyntax.AscribeExpr _) ->
     let e, ty' = infer_expr ctx e in
     if Rsyntax.equal_expr_ty ty ty'
     then
       e
     else
       error ~loc (ExprTypeMismatch (ty, ty'))

(** [check_comp ctx c ty] checks that computation [c] has type [ty] in context [ctx].
    It returns the processed computation [c]. *)
and check_comp ctx ({Location.data=c'; loc} as c) ty =
  let locate = Location.locate ~loc in
  match c' with

  | Dsyntax.Return e ->
    let (Rsyntax.CompTy (ty, _)) = ty in
    let e = check_expr ctx e ty in
    locate (Rsyntax.Return e)

  | Dsyntax.Sequence (x, c1, c2) ->
     let c1, t1 = infer_comp ctx c1 in
     let c2 = check_comp (extend x (Rsyntax.purify t1) ctx) c2 ty in
     locate (Rsyntax.Sequence (c1, c2))

  | (Dsyntax.Apply _ | Dsyntax.AscribeComp _) ->
     let c, ty' = infer_comp ctx c in
     if Rsyntax.equal_comp_ty ty ty'
     then
       c
     else
       error ~loc (CompTypeMismatch (ty, ty'))

and toplevel ~quiet ctx {Location.data=d'; loc} =
  let ctx, d' =
    match d' with

    | Dsyntax.TopLoad lst ->
       let ctx, lst = topfile ~quiet ctx lst in
       ctx, Rsyntax.TopLoad lst

    | Dsyntax.TopLet (x, c) ->
       let c, ty = infer_comp ctx c in
       let ctx = extend x (Rsyntax.purify ty) ctx in
       ctx, Rsyntax.TopLet (x, Rsyntax.purify ty, c)

    | Dsyntax.TopComp c ->
       let c, ty = infer_comp ctx c in
       ctx, Rsyntax.TopComp (c, ty)

    | Dsyntax.DeclOperation (op, ty1, ty2) ->
       let ty1 = expr_ty ty1
       and ty2 = Rsyntax.pollute (Rsyntax.purely (expr_ty ty2)) op in
       let ctx = extend op (Rsyntax.Arrow (ty1, ty2)) ctx in
       ctx, Rsyntax.DeclOperation (op, ty1, ty2)

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
