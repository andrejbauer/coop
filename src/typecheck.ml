(** Terminus type checking. *)

(** Typing context *)
type context = (Name.ident * Rsyntax.expr_ty) list

(** Initial typing context *)
let initial = []

(** Type errors *)
type error =
  | InvalidIndex of int
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

  | InvalidIndex k -> Format.fprintf ppf "invalid de Bruijn index %d, please report" k

  | ExprTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type %t but has type %t"
                        (Rsyntax.print_expr_ty ty_expected)
                        (Rsyntax.print_expr_ty ty_actual)

  | CompTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type %t but has type %t"
                        (Rsyntax.print_comp_ty ty_expected)
                        (Rsyntax.print_comp_ty ty_actual)

  | TypeExpectedButFunction ty ->
     Format.fprintf ppf "this expression is a function but should have type %t"
                        (Rsyntax.print_expr_ty ty)

  | FunctionExpected ty ->
     Format.fprintf ppf "this expression should be a function but has type %t"
                        (Rsyntax.print_expr_ty ty)

  | CannotInferArgument x ->
     Format.fprintf ppf "cannot infer the type of %t" (Name.print_ident x)

(** Extend the context with the type of deBruijn index 0 *)
let extend x ty ctx = (x, ty) :: ctx

(** Lookup the type of a deBruijn index *)
let lookup ~loc k ctx =
  try
    snd (List.nth ctx k)
  with
    | Failure _ -> error ~loc (InvalidIndex k)

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
  let e', ty =
    match e' with
    | Dsyntax.Var k ->
       let ty = lookup ~loc k ctx in
       Rsyntax.Var k, ty

    | Dsyntax.Numeral n ->
       Rsyntax.Numeral n, Rsyntax.Int

    | Dsyntax.Lambda (x, Some t, c) ->
       let t = expr_ty t in
       let ctx  = extend x t ctx in
       let c, c_ty = infer_comp ctx c in
       Rsyntax.Lambda c,
       Rsyntax.Arrow (t, c_ty)

    | Dsyntax.Lambda (x, None, _) ->
       error ~loc (CannotInferArgument x)
  in
  Location.locate ~loc e', ty

(** [infer_comp ctx c] infers the type [ty] of a computation [c]. It returns
    the processed computation [c] and its type [ty].  *)
and infer_comp (ctx : context) {Location.data=c'; loc} =
  let c', ty =
    match c' with

    | Dsyntax.Return e ->
       let e, e_ty = infer_expr ctx e in
       Rsyntax.Return e, Rsyntax.purely e_ty

    | Dsyntax.Sequence (x, c1, c2) ->
       let c1, c1_ty = infer_comp ctx c1 in
       let ctx = extend x (Rsyntax.purify c1_ty) ctx in
       let c2, c2_ty = infer_comp ctx c2 in
       Rsyntax.Sequence (c1, c2), c2_ty

    | Dsyntax.Apply (e1, e2) ->
       let e1, t1 = infer_expr ctx e1 in
       begin
         match t1 with
         | Rsyntax.Arrow (u1, u2) ->
            let e2 = check_expr ctx e2 u1 in
            Rsyntax.Apply (e1, e2),
            u2

         | Rsyntax.Int -> error ~loc (FunctionExpected t1)
       end
  in
  Location.locate ~loc c', ty


(** [check_expr ctx e ty] checks that expression [e] has type [ty] in context [ctx].
    It returns the processed expression [e]. *)
and check_expr (ctx : context) ({Location.data=e'; loc} as e) ty =
  match e' with

  | Dsyntax.Lambda (x, None, e) ->
     begin
       match ty with
       | Rsyntax.Arrow (t, u) ->
          let c = check_comp (extend x t ctx) e u in
          Location.locate ~loc (Rsyntax.Lambda c)
       | Rsyntax.Int -> error ~loc (TypeExpectedButFunction ty)
     end

  | Dsyntax.Numeral k ->
     begin match ty with
     | Rsyntax.Int -> Location.locate ~loc (Rsyntax.Numeral k)
     | Rsyntax.Arrow _ -> error ~loc (TypeExpectedButFunction ty)
     end

  | (Dsyntax.Lambda (_, Some _, _) | Dsyntax.Var _) ->
     let e, ty' = infer_expr ctx e in
     if Rsyntax.equal_expr_ty ty ty'
     then
       e
     else
       error ~loc (ExprTypeMismatch (ty, ty'))

(** [check_comp ctx c ty] checks that computation [c] has type [ty] in context [ctx].
    It returns the processed computation [c]. *)
and check_comp (ctx : context) ({Location.data=c'; loc} as c) ty : Rsyntax.comp =
  match c' with

  | Dsyntax.Return e ->
    let (Rsyntax.CompTy (ty, _)) = ty in
    let e = check_expr ctx e ty in
    Location.locate ~loc (Rsyntax.Return e)

  | Dsyntax.Sequence (x, c1, c2) ->
     let c1, t1 = infer_comp ctx c1 in
     check_comp (extend x (Rsyntax.purify t1) ctx) c2 ty

  | Dsyntax.Apply _ ->
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
