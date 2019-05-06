(** Terminus type checking. *)

(** Typing context *)
type context =
  { ctx_ops : (Syntax.expr_ty * Syntax.expr_ty) Name.Map.t
  ; ctx_idents : (Name.t * Syntax.expr_ty) list
  }

(** Initial typing context *)
let initial =
  { ctx_ops = Name.Map.empty
  ; ctx_idents = []
  }

(** Type errors *)
type error =
  | InvalidName of Name.t
  | InvalidOperation of Name.t
  | PattTypeMismatch of Syntax.expr_ty
  | ExprTypeMismatch of Syntax.expr_ty * Syntax.expr_ty
  | ExprTypeExpected
  | CompTypeMismatch of Syntax.comp_ty * Syntax.comp_ty
  | TypeExpectedButFunction of Syntax.expr_ty
  | TypeExpectedButTuple of Syntax.expr_ty
  | TupleTooShort of Syntax.expr_ty
  | TupleTooLong of Syntax.expr_ty
  | FunctionExpected of Syntax.expr_ty
  | ComodelExpected of Syntax.expr_ty
  | CannotInferArgument of Name.t
  | CannotInferMatch
  | DuplicateOperation of Name.t
  | UnhandledOperation of Name.t

exception Error of error Location.located

(** [error ~loc err] raises the given type-checking error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

let print_error err ppf =
  match err with

  | InvalidName x -> Format.fprintf ppf "invalid name %t, please report" (Name.print x)

  | InvalidOperation op -> Format.fprintf ppf "invalid operation %t, please report" (Name.print op)

  | PattTypeMismatch ty_expected ->
     Format.fprintf ppf "this pattern should have type@ %t"
                        (Syntax.print_expr_ty ty_expected)

  | ExprTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type@ %t but has type@ %t"
                        (Syntax.print_expr_ty ty_expected)
                        (Syntax.print_expr_ty ty_actual)

  | CompTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this computation should have type@ %t but has type@ %t"
                        (Syntax.print_comp_ty ty_expected)
                        (Syntax.print_comp_ty ty_actual)

  | ExprTypeExpected ->
     Format.fprintf ppf "this type should be pure"

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

  | ComodelExpected ty ->
     Format.fprintf ppf "this expression should be a comodel but has type@ %t"
                        (Syntax.print_expr_ty ty)

  | CannotInferArgument x ->
     Format.fprintf ppf "cannot infer the type of@ %t" (Name.print x)

  | CannotInferMatch ->
     Format.fprintf ppf "cannot infer the type of this match statement"

  | DuplicateOperation op ->
     Format.fprintf ppf "operation %t is defined twice" (Name.print op)

  | UnhandledOperation op ->
     Format.fprintf ppf "uhnadled operation %t" (Name.print op)

(** Extend the context with the type of deBruijn index 0 *)
let extend x ty ctx = { ctx with ctx_idents = (x, ty) :: ctx.ctx_idents }

let declare_operation op ty1 ty2 ctx = { ctx with ctx_ops = Name.Map.add op (ty1, ty2) ctx.ctx_ops }

let rec extends xts ctx =
  match xts with
  | [] -> ctx
  | (x, t) :: xts -> extends xts (extend x t ctx)

(** Lookup the index and the type of a name *)
let lookup ~loc x {ctx_idents;_} =
  let rec fold k = function
    | [] -> error ~loc (InvalidName x)
    | (y,t) :: _ when Name.equal x y -> (k, t)
    | _ :: lst -> fold (k+1) lst
  in
  fold 0 ctx_idents

let lookup_operation ~loc op {ctx_ops;_} =
  match Name.Map.find op ctx_ops with
  | None -> error ~loc (InvalidOperation op)
  | Some (ty1, ty2) -> (ty1, ty2)

(** Check that a type is valid. Retrn the processed type. *)
let rec expr_ty {Location.data=t'; loc} =
  match t' with

  | Desugared.Int -> Syntax.Int

  | Desugared.Product lst ->
     let lst = List.map expr_ty lst in
     Syntax.Product lst

  | Desugared.Arrow (t1, t2) ->
     let t1 = expr_ty t1
     and t2 = comp_ty t2 in
     Syntax.Arrow (t1, t2)

  | Desugared.ComodelTy (sgn1, t, sgn2) ->
     let t = expr_ty t in
     Syntax.ComodelTy (sgn1, t, sgn2)

  | Desugared.CompTy _ ->
     error ~loc ExprTypeExpected

and comp_ty ({Location.data=t'; loc} as t) =
  match t' with

  | Desugared.CompTy (t, sgn) ->
     let t = expr_ty t in
     Syntax.CompTy (t, sgn)

  | (Desugared.Int | Desugared.Product _ | Desugared.Arrow _ | Desugared.ComodelTy _) ->
     let t = expr_ty t in
     Syntax.CompTy (t, Syntax.empty_signature)


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

    | ((Desugared.PattTuple _, (Syntax.Int | Syntax.Arrow _ | Syntax.ComodelTy _)) |
       (Desugared.PattNumeral _, (Syntax.Product _ | Syntax.Arrow _ | Syntax.ComodelTy _))) ->
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

(** Make sure that the dirt of [ty] is a subsignature of [sgn], or report an error. *)
let check_dirt ~loc (Syntax.CompTy (_, sgn1)) sgn2 =
  match Name.Set.choose_diff sgn1 sgn2 with
  | None -> ()
  | Some op -> error ~loc (UnhandledOperation op)


(** [infer_expr ctx e] infers the expression type [ty] of an expression [e]. It
   returns the processed expression [e] and its type [ty]. *)
let rec infer_expr (ctx : context) {Location.data=e'; loc} =
  let locate = Location.locate ~loc in
  match e' with
  | Desugared.Var x ->
     let k, ty = lookup ~loc x ctx in
     locate (Syntax.Var k), ty

  | Desugared.AscribeExpr (e, t) ->
     let t = expr_ty t in
     let e = check_expr ctx e t in
     e, t

  | Desugared.Numeral n ->
     locate (Syntax.Numeral n), Syntax.Int

  | Desugared.Tuple lst ->
     let lst = List.map (infer_expr ctx) lst in
     locate (Syntax.Tuple (List.map fst lst)),  Syntax.Product (List.map snd lst)

  | Desugared.Lambda ((x, Some t), c) ->
     let t = expr_ty t in
     let ctx = extend x t ctx in
     let c, c_ty = infer_comp ctx c in
     locate (Syntax.Lambda c), Syntax.Arrow (t, c_ty)

  | Desugared.Lambda ((x, None), _) ->
     error ~loc (CannotInferArgument x)

  | Desugared.Comodel (e, coops) ->
     let e, e_ty = infer_expr ctx e in
     let coops, sgn1, sgn2 = infer_coops ~loc ctx e_ty coops in
     locate (Syntax.Comodel (e, coops)), Syntax.ComodelTy (sgn1, e_ty, sgn2)

(** [infer_comp ctx c] infers the type [ty] of a computation [c]. It returns
    the processed computation [c] and its type [ty].  *)
and infer_comp (ctx : context) {Location.data=c'; loc} =
  let locate = Location.locate ~loc in
  match c' with

  | Desugared.Val e ->
     let e, e_ty = infer_expr ctx e in
     locate (Syntax.Val e), Syntax.pure e_ty

  | Desugared.AscribeComp (c, t) ->
     let t = comp_ty t in
     let c = check_comp ctx c t in
     c, t

  | Desugared.Let (p, c1, c2) ->
     let c1, (Syntax.CompTy (c1_ty, c1_sgn)) = infer_comp ctx c1 in
     let ctx, p = extend_pattern ctx p c1_ty in
     let c2, c2_ty = infer_comp ctx c2 in
     let c2_ty = Syntax.pollute c2_ty c1_sgn in
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

       | (Syntax.Int | Syntax.Product _ | Syntax.ComodelTy _) ->
          error ~loc:(e1.Location.loc) (FunctionExpected t1)
     end

  | Desugared.Operation (op, e) ->
     let ty1, ty2 = lookup_operation ~loc op ctx in
     let e = check_expr ctx e ty1 in
     let e_ty = Syntax.op_ty ty2 op in
     locate (Syntax.Operation (op, e)), e_ty

  | Desugared.Using (cmdl, c, (px, pw, fin)) ->
     let cmdl, (sig1, w_ty, sig2) = infer_comodel ctx cmdl in
     let c, (Syntax.CompTy (c_ty', _) as c_ty) = infer_comp ctx c in
     check_dirt ~loc c_ty sig1 ;
     let ctx, px = extend_pattern ctx px c_ty' in
     let ctx, pw = extend_pattern ctx pw w_ty in
     let fin, fin_ty = infer_comp ctx fin in
     locate (Syntax.Using (cmdl, c, (px, pw, fin))), fin_ty

and infer_match_clauses ~loc ctx patt_ty lst =
  match lst with
  | [] -> error ~loc CannotInferMatch
  | (p, c) :: lst ->
     let ctx, p = extend_pattern ctx p patt_ty in
     let c, c_ty = infer_comp ctx c in
     let lst = check_match_clauses ctx patt_ty c_ty lst in
     ((p, c) :: lst), c_ty

and infer_coops ~loc ctx w_ty lst =
  let rec fold coops sgn1 sgn2 = function

    | [] ->
       let coops = List.rev coops in
       coops, sgn1, sgn2

    | (op, px, pw, c) :: lst ->
       if Name.Set.mem op sgn1 then
         error ~loc (DuplicateOperation op)
       else
         let (x_ty, op_ty) = lookup_operation ~loc op ctx in
         let ctx, px = extend_pattern ctx px x_ty in
         let ctx, pw = extend_pattern ctx pw w_ty in
         let c, (Syntax.CompTy (_, c_sgn) as c_ty) = infer_comp ctx c in
         let c_required = Syntax.pure (Syntax.Product [op_ty; w_ty]) in
         if not (Syntax.comp_subty c_required c_ty) then
           error ~loc (CompTypeMismatch (c_required, c_ty)) ;
         let coops = (op, px, pw, c) :: coops
         and sgn1 = Name.Set.add op sgn1
         and sgn2 = Name.Set.union sgn2 c_sgn in
         fold coops sgn1 sgn2 lst
  in
  fold [] Name.Set.empty Name.Set.empty lst

and infer_comodel ctx cmdl =
  let e, e_ty = infer_expr ctx cmdl in
  match e_ty with

  | Syntax.ComodelTy (sgn1, t, sgn2) -> e, (sgn1, t, sgn2)

  | Syntax.Int | Syntax.Product _ | Syntax.Arrow _ ->
     error ~loc:cmdl.Location.loc (ComodelExpected e_ty)

(** [check_expr ctx e ty] checks that expression [e] has type [ty] in context [ctx].
    It returns the processed expression [e]. *)
and check_expr (ctx : context) ({Location.data=e'; loc} as e) ty =
  let locate = Location.locate ~loc in
  match e' with

  | Desugared.Lambda ((x, None), e) ->
     begin
       match ty with
       | Syntax.Arrow (t, u) ->
          let c = check_comp (extend x t ctx) e u in
          locate (Syntax.Lambda c)
       | (Syntax.Int | Syntax.Product _ | Syntax.ComodelTy _) ->
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

       | (Syntax.Int | Syntax.Arrow _ | Syntax.ComodelTy _) ->
          error ~loc (TypeExpectedButTuple ty)
     end

  | (Desugared.Numeral _ | Desugared.Lambda ((_, Some _), _) | Desugared.Var _ |
     Desugared.AscribeExpr _ | Desugared.Comodel _) ->
     let e, ty' = infer_expr ctx e in
     if Syntax.expr_subty ty' ty
     then
       e
     else
       error ~loc (ExprTypeMismatch (ty, ty'))

(** [check_comp ctx c ty] checks that computation [c] has computation type [ty] in context [ctx].
    It returns the processed computation [c]. *)
and check_comp ctx ({Location.data=c'; loc} as c) check_ty =
  let (Syntax.CompTy (check_ty', check_sgn)) = check_ty in
  let locate = Location.locate ~loc in
  match c' with

  | Desugared.Val e ->
    let e = check_expr ctx e check_ty' in
    locate (Syntax.Val e)

  | Desugared.Match (e, lst) ->
     let e, e_ty = infer_expr ctx e in
     let lst = check_match_clauses ctx e_ty check_ty lst in
     locate (Syntax.Match (e, lst))

  | Desugared.Let (p, c1, c2) ->
     let c1, (Syntax.CompTy (c1_ty', _) as c1_ty) = infer_comp ctx c1 in
     check_dirt ~loc c1_ty check_sgn ;
     let ctx, p = extend_pattern ctx p c1_ty' in
     let c2 = check_comp ctx c2 check_ty in
     locate (Syntax.Let (p, c1, c2))

  | Desugared.Using (cmdl, c, (px, pw, fin)) ->
     let cmdl, (sig1, w_ty, sig2) = infer_comodel ctx cmdl in
     let c, (Syntax.CompTy (c_ty', _) as c_ty) = infer_comp ctx c in
     check_dirt ~loc c_ty sig1 ;
     let ctx, px = extend_pattern ctx px c_ty' in
     let ctx, pw = extend_pattern ctx pw w_ty in
     let fin = check_comp ctx fin check_ty in
     locate (Syntax.Using (cmdl, c, (px, pw, fin)))

  | (Desugared.Apply _ | Desugared.AscribeComp _ | Desugared.Operation _) ->
     let c, c_ty = infer_comp ctx c in
     if Syntax.comp_subty c_ty check_ty
     then
       c
     else
       error ~loc (CompTypeMismatch (check_ty, c_ty))

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
       let c, (Syntax.CompTy (c_ty', _) as c_ty) = infer_comp ctx c in
       check_dirt ~loc c_ty Syntax.empty_signature ;
       let ctx, p, xts = top_extend_pattern ctx p c_ty' in
       ctx, Syntax.TopLet (p, xts, c)

    | Desugared.TopComp c ->
       let c, (Syntax.CompTy (c_ty', _) as c_ty) = infer_comp ctx c in
       check_dirt ~loc c_ty Syntax.empty_signature ;
       ctx, Syntax.TopComp (c, c_ty')

    | Desugared.DeclOperation (op, ty1, ty2) ->
       let ty1 = expr_ty ty1
       and ty2 = expr_ty ty2 in
       let ctx = declare_operation op ty1 ty2 ctx in
       ctx, Syntax.DeclOperation (op, ty1, ty2)

    | Desugared.External (x, t, s) ->
       let t = expr_ty t in
       let ctx = extend x t ctx in
       ctx, Syntax.External (x, t, s)

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
