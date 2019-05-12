(** Coop type checking. *)

(** Typing context *)
type context =
  { ctx_operations : (Syntax.expr_ty * Syntax.expr_ty) Name.Map.t
  ; ctx_signals : Syntax.expr_ty Name.Map.t
  ; ctx_idents : (Name.t * Syntax.expr_ty) list
  }

(** Initial typing context *)
let initial =
  { ctx_operations = Name.Map.empty
  ; ctx_signals = Name.Map.empty
  ; ctx_idents = []
  }

(** Type errors *)
type error =
  | InvalidName of Name.t
  | InvalidOperation of Name.t
  | InvalidSignal of Name.t
  | PattTypeMismatch of Syntax.expr_ty
  | ExprTypeMismatch of Syntax.expr_ty * Syntax.expr_ty
  | ExprTypeExpected
  | CompTypeMismatch of Syntax.comp_ty * Syntax.comp_ty
  | TypeExpectedButFunction of Syntax.expr_ty
  | TypeExpectedButTuple of Syntax.expr_ty
  | TypeExpectedButUnit of Syntax.expr_ty
  | TupleTooShort of Syntax.expr_ty
  | TupleTooLong of Syntax.expr_ty
  | FunctionExpected of Syntax.expr_ty
  | ComodelExpected of Syntax.expr_ty
  | CannotInferArgument
  | CannotInferMatch
  | DuplicateOperation of Name.t
  | DuplicateSignal of Name.t
  | UnhandledOperationsSignals of Name.Set.t * Name.Set.t

exception Error of error Location.located

(** [error ~loc err] raises the given type-checking error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

let print_error err ppf =
  match err with

  | InvalidName x -> Format.fprintf ppf "invalid name %t, please report" (Name.print x)

  | InvalidOperation op ->
     Format.fprintf ppf "invalid operation %t, please report"
       (Name.print op)

  | InvalidSignal sgl ->
     Format.fprintf ppf "invalid signal %t, please report"
       (Name.print sgl)

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

  | TypeExpectedButUnit ty ->
     Format.fprintf ppf "this expression is the unit but should have type@ %t"
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

  | CannotInferArgument ->
     Format.fprintf ppf "cannot infer the type of this argument"

  | CannotInferMatch ->
     Format.fprintf ppf "cannot infer the type of this match statement"

  | DuplicateOperation op ->
     Format.fprintf ppf "operation %t is defined twice"
       (Name.print op)

  | DuplicateSignal sgl ->
     Format.fprintf ppf "signal %t is intercepted more than once"
       (Name.print sgl)

  | UnhandledOperationsSignals (ops, sgs) ->
     let ops = Name.Set.elements ops in
     let sgs = Name.Set.elements sgs in
     Format.fprintf ppf "the following %s are potentially unhandled:@ %t"
       (match ops, sgs with
        | [], [] -> assert false
        | _::_, [] -> "operations"
        | [], _::_ -> "signals"
        | _::_, _::_ -> "operations and signals")
       (Print.sequence (Name.print ~parentheses:true) "," (ops @ sgs))

(** Extend the context with the type of deBruijn index 0 *)
let extend_ident x ty ctx =
  { ctx with ctx_idents = (x, ty) :: ctx.ctx_idents }

let rec extend_idents xts ctx =
  match xts with
  | [] -> ctx
  | (x, t) :: xts -> extend_idents xts (extend_ident x t ctx)

let declare_operation op ty1 ty2 ctx =
  { ctx with ctx_operations = Name.Map.add op (ty1, ty2) ctx.ctx_operations }

let declare_signal sgl ty ctx =
  { ctx with ctx_signals = Name.Map.add sgl ty ctx.ctx_signals }

(** Lookup the index and the type of a name *)
let lookup ~loc x {ctx_idents;_} =
  let rec fold k = function
    | [] -> error ~loc (InvalidName x)
    | (y,t) :: _ when Name.equal x y -> (k, t)
    | _ :: lst -> fold (k+1) lst
  in
  fold 0 ctx_idents

let lookup_operation ~loc op {ctx_operations;_} =
  match Name.Map.find op ctx_operations with
  | None -> error ~loc (InvalidOperation op)
  | Some (ty1, ty2) -> (ty1, ty2)

let lookup_signal ~loc sgl {ctx_signals;_} =
  match Name.Map.find sgl ctx_signals with
  | None -> error ~loc (InvalidSignal sgl)
  | Some ty -> ty

let signature Desugared.{sig_ops; sig_sgs} = Syntax.{sig_ops; sig_sgs}


(**** Subtyping ****)

let rec join_expr_ty ~loc t1 t2 =
  match t1, t2 with

  | Syntax.SignalTy, t2 -> t2

  | t1, Syntax.SignalTy -> t1

  | Syntax.Int, Syntax.Int -> Syntax.Int

  | Syntax.Bool, Syntax.Bool -> Syntax.Bool

  | Syntax.Product ts1, Syntax.Product ts2 ->
     let rec fold ts ts1 ts2 =
       match ts1, ts2 with
       | [], [] ->
          let ts = List.rev ts in
          Syntax.Product ts
       | t1::ts1, t2::ts2 ->
          let t = join_expr_ty ~loc t1 t2 in
          fold (t :: ts) ts1 ts2
       | [], _::_ | _::_, [] ->
          error ~loc (ExprTypeMismatch (t2, t1))
     in
     fold [] ts1 ts2

  | Syntax.Arrow (u1, t1), Syntax.Arrow (u2, t2) ->
     let u = meet_expr_ty ~loc u1 u2 in
     let t = join_comp_ty ~loc t1 t2 in
     Syntax.Arrow (u, t)

  | Syntax.ComodelTy (ops1, t1, sig1), Syntax.ComodelTy (ops2, t2, sig2) ->
     let ops = Name.Set.inter ops1 ops2
     and t = meet_expr_ty ~loc t1 t2
     and sgn = Syntax.join_signature sig1 sig2 in
     Syntax.ComodelTy  (ops, t, sgn)

  | (Syntax.Int | Syntax.Bool | Syntax.Product _ | Syntax.Arrow _ | Syntax.ComodelTy _), _ ->
     error ~loc (ExprTypeMismatch (t2, t2))

and meet_expr_ty ~loc t1 t2 =
  match t1, t2 with

  | Syntax.SignalTy, _ -> Syntax.SignalTy

  | _, Syntax.SignalTy -> Syntax.SignalTy

  | Syntax.Int, Syntax.Int -> Syntax.Int

  | Syntax.Bool, Syntax.Bool -> Syntax.Bool

  | Syntax.Product ts1, Syntax.Product ts2 ->
     let rec fold ts ts1 ts2 =
       match ts1, ts2 with
       | [], [] ->
          let ts = List.rev ts in
          Syntax.Product ts
       | t1::ts1, t2::ts2 ->
          let t = meet_expr_ty ~loc t1 t2 in
          fold (t :: ts) ts1 ts2
       | [], _::_ | _::_, [] ->
          error ~loc (ExprTypeMismatch (t2, t1))
     in
     fold [] ts1 ts2

  | Syntax.Arrow (u1, t1), Syntax.Arrow (u2, t2) ->
     let u = join_expr_ty ~loc u1 u2 in
     let t = meet_comp_ty ~loc t1 t2 in
     Syntax.Arrow (u, t)

  | Syntax.ComodelTy (ops1, t1, sig1), Syntax.ComodelTy (ops2, t2, sig2) ->
     let ops = Name.Set.union ops1 ops2
     and t = join_expr_ty ~loc t1 t2
     and sgn = Syntax.meet_signature sig1 sig2 in
     Syntax.ComodelTy  (ops, t, sgn)

  | (Syntax.Int | Syntax.Bool | Syntax.Product _ | Syntax.Arrow _ | Syntax.ComodelTy _), _ ->
     error ~loc (ExprTypeMismatch (t2, t2))

and join_comp_ty ~loc (Syntax.CompTy(t1, sig1)) (Syntax.CompTy(t2,sig2)) =
  let t = join_expr_ty ~loc t1 t2
  and sgn = Syntax.join_signature sig1 sig2 in
  Syntax.CompTy (t, sgn)

and meet_comp_ty ~loc (Syntax.CompTy(t1, sig1)) (Syntax.CompTy(t2,sig2)) =
  let t = meet_expr_ty ~loc t1 t2
  and sgn = Syntax.meet_signature sig1 sig2 in
  Syntax.CompTy (t, sgn)

(**** Type checking ****)

(** Check that a type is valid. Retrn the processed type. *)
let rec expr_ty {Location.data=t'; loc} =
  match t' with

  | Desugared.Int -> Syntax.Int

  | Desugared.Bool -> Syntax.Bool

  | Desugared.Product lst ->
     let lst = List.map expr_ty lst in
     Syntax.Product lst

  | Desugared.Arrow (t1, t2) ->
     let t1 = expr_ty t1
     and t2 = comp_ty t2 in
     Syntax.Arrow (t1, t2)

  | Desugared.ComodelTy (ops, t, sgn2) ->
     let t = expr_ty t
     and sgn2 = signature sgn2 in
     Syntax.ComodelTy (ops, t, sgn2)

  | Desugared.CompTy _ ->
     error ~loc ExprTypeExpected

and comp_ty ({Location.data=t'; loc} as t) =
  match t' with

  | Desugared.CompTy (t, sgn) ->
     let t = expr_ty t
     and sgn = signature sgn in
     Syntax.CompTy (t, sgn)

  | (Desugared.Int | Desugared.Bool | Desugared.Product _ |
     Desugared.Arrow _ | Desugared.ComodelTy _) ->
     let t = expr_ty t in
     Syntax.CompTy (t, Syntax.empty_signature)


(** Typecheck a pattern, return processed pattern and the list of identifiers
   and types bound by the pattern *)
let check_pattern patt ty =
  let rec fold xts {Location.data=p'; loc} t =
    match p', t with

    | _, Syntax.SignalTy ->
       error ~loc (PattTypeMismatch ty)

    | Desugared.PattAnonymous, _ ->
       Syntax.PattAnonymous, xts

    | Desugared.PattVar x, _ ->
       Syntax.PattVar, (x, t) :: xts

    | Desugared.PattNumeral n, Syntax.Int ->
       Syntax.PattNumeral n, xts

    | Desugared.PattBoolean b, Syntax.Bool ->
       Syntax.PattBoolean b, xts

    | Desugared.PattTuple ps, Syntax.Product ts ->
       fold_tuple ~loc xts [] ps ts

    | Desugared.PattNumeral _, (Syntax.Bool | Syntax.Product _ | Syntax.Arrow _ | Syntax.ComodelTy _)
    | Desugared.PattBoolean _, (Syntax.Int | Syntax.Product _ | Syntax.Arrow _ | Syntax.ComodelTy _)
    | Desugared.PattTuple _, (Syntax.Int | Syntax.Bool | Syntax.Arrow _ | Syntax.ComodelTy _) ->
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
  let ctx = extend_idents xts ctx in
  ctx, p

(** Extend the context by typechecking the pattern against the type,
    also return the list of bound names with their types. *)
let top_extend_pattern ctx p t =
  let p, xts = check_pattern p t in
  let ctx = extend_idents xts ctx in
  ctx, p, xts

(** Make sure that the dirt of the first type is a subsignature of [sgn], or report an error. *)
let check_dirt ~loc
  (Syntax.CompTy (_, Syntax.{sig_ops=ops1; sig_sgs=sgs1}))
  Syntax.{sig_ops=ops2; sig_sgs=sgs2} =
  let ops = Name.Set.diff ops1 ops2 in
  let sgs = Name.Set.diff sgs1 sgs2 in
  if not (Name.Set.is_empty ops && Name.Set.is_empty sgs) then
    error ~loc (UnhandledOperationsSignals (ops, sgs))


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

  | Desugared.Boolean b ->
     locate (Syntax.Boolean b), Syntax.Bool

  | Desugared.Tuple lst ->
     let lst = List.map (infer_expr ctx) lst in
     locate (Syntax.Tuple (List.map fst lst)),  Syntax.Product (List.map snd lst)

  | Desugared.Lambda ((p, Some t), c) ->
     let t = expr_ty t in
     let ctx, p = extend_pattern ctx p t in
     let c, c_ty = infer_comp ctx c in
     locate (Syntax.Lambda (p, c)), Syntax.Arrow (t, c_ty)

  | Desugared.Lambda ((p, None), _) ->
     error ~loc:p.Location.loc CannotInferArgument

  | Desugared.Comodel (t, coops) ->
     let t = expr_ty t in
     let coops, sgn1, sgn2 = infer_coops ~loc ctx t coops in
     locate (Syntax.Comodel coops), Syntax.ComodelTy (sgn1, t, sgn2)

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

       | (Syntax.SignalTy | Syntax.Int | Syntax.Bool | Syntax.Product _ | Syntax.ComodelTy _) ->
          error ~loc:(e1.Location.loc) (FunctionExpected t1)
     end

  | Desugared.Operation (op, e) ->
     let ty1, ty2 = lookup_operation ~loc op ctx in
     let e = check_expr ctx e ty1 in
     let e_ty = Syntax.operation_ty ty2 op in
     locate (Syntax.Operation (op, e)), e_ty

  | Desugared.Signal (sgl, e) ->
     let e_ty = lookup_signal ~loc sgl ctx in
     let e = check_expr ctx e e_ty in
     let ty = Syntax.signal_ty sgl in
     locate (Syntax.Signal (sgl, e)), ty

  | Desugared.Using (cmdl, w, c, fin) ->
     let cmdl, (ops, w_ty, Syntax.{sig_ops=cmdl_ops; sig_sgs=cmdl_sgs}) =
       infer_comodel ctx cmdl in
     let w = check_expr ctx w w_ty in
     let c, (Syntax.CompTy(x_ty, _) as c_ty) = infer_comp ctx c in
     let fin, fin_sgs, fin_ty = infer_finally ~loc ctx x_ty w_ty fin in
     let cmdl_sig = Syntax.{sig_ops=cmdl_ops; sig_sgs = Name.Set.diff cmdl_sgs fin_sgs} in
     let fin_ty = Syntax.pollute fin_ty cmdl_sig in
     check_dirt ~loc c_ty Syntax.{sig_ops=ops; sig_sgs=fin_sgs} ;
     locate (Syntax.Using (cmdl, w, c, fin)), fin_ty

and infer_match_clauses ~loc ctx patt_ty lst =
  match lst with
  | [] -> error ~loc CannotInferMatch
  | (p, c) :: lst ->
     let ctx, p = extend_binder ctx p patt_ty in
     let c, c_ty = infer_comp ctx c in
     let rec fold clauses ty = function
       | [] -> List.rev clauses, ty
       | (p, c) :: lst ->
          let ctx, p = extend_binder ctx p patt_ty in
          let c, c_ty = infer_comp ctx c in
          let ty = join_comp_ty ~loc:c.Location.loc c_ty ty in
          fold ((p,c) :: clauses) ty lst
     in
     let clauses, ty = fold [] c_ty lst in
     ((p, c) :: clauses), ty

and infer_coops ~loc ctx w_ty lst =
  let rec fold coops ops sgn2 = function

    | [] ->
       let coops = List.rev coops in
       coops, ops, sgn2

    | (op, px, pw, c) :: lst ->
       if Name.Set.mem op ops then
         error ~loc (DuplicateOperation op)
       else
         let (x_ty, op_ty) = lookup_operation ~loc op ctx in
         let ctx, px = extend_binder ctx px x_ty in
         let ctx, pw = extend_binder ctx pw w_ty in
         let c, (Syntax.CompTy (_, c_sgn) as c_ty) = infer_comp ctx c in
         let c_required = Syntax.pure (Syntax.Product [op_ty; w_ty]) in
         if not (Syntax.comp_subty c_required c_ty) then
           error ~loc (CompTypeMismatch (c_required, c_ty)) ;
         let coops = (op, px, pw, c) :: coops
         and ops = Name.Set.add op ops
         and sgn2 = Syntax.join_signature sgn2 c_sgn in
         fold coops ops sgn2 lst
  in
  fold [] Name.Set.empty Syntax.empty_signature lst

and infer_comodel ctx cmdl =
  let e, e_ty = infer_expr ctx cmdl in
  match e_ty with

  | Syntax.ComodelTy (ops, t, sgn2) -> e, (ops, t, sgn2)

  | Syntax.SignalTy | Syntax.Int | Syntax.Bool | Syntax.Product _ | Syntax.Arrow _ ->
     error ~loc:cmdl.Location.loc (ComodelExpected e_ty)

and infer_finally ~loc ctx x_ty w_ty Desugared.{fin_val; fin_signals} =
  let fin_val, c_ty =
    let (px, pw, c) = fin_val in
    let ctx, px = extend_binder ctx px x_ty in
    let ctx, pw = extend_binder ctx pw w_ty in
    let c, c_ty = infer_comp ctx c in
    (px, pw, c), c_ty
  in
  let fin_signals, fin_sgs, fin_ty =
    let rec fold fs sgs ty = function
      | [] ->
         let fs = List.rev fs in
         fs, sgs, ty
      | (sg, px, pw, c_sg) :: lst ->
         begin
           if List.exists (fun (sg', _, _, _) -> Name.equal sg sg') fs then error ~loc (DuplicateSignal sg) ;
           let x_ty = lookup_signal ~loc sg ctx in
           let ctx, px = extend_binder ctx px x_ty in
           let ctx, pw = extend_binder ctx pw w_ty in
           let c_sg, sg_ty = infer_comp ctx c_sg in
           let ty = join_comp_ty ~loc:c_sg.Location.loc ty sg_ty in
           let sgs = Name.Set.add sg sgs in
           fold ((sg, px, pw, c_sg) :: fs) sgs ty lst
         end
    in
    fold [] Name.Set.empty c_ty fin_signals
  in
  Syntax.{fin_val; fin_signals}, fin_sgs, fin_ty


and extend_binder ctx (p, topt) t =
  match topt with
  | None -> extend_pattern ctx p t
  | Some t' ->
     let t' = expr_ty t' in
     if not (Syntax.expr_subty t t') then
       error ~loc:p.Location.loc (ExprTypeMismatch (t, t')) ;
     extend_pattern ctx p t'

(** [check_expr ctx e ty] checks that expression [e] has type [ty] in context [ctx].
    It returns the processed expression [e]. *)
and check_expr (ctx : context) ({Location.data=e'; loc} as e) ty =
  let locate = Location.locate ~loc in
  match e' with

  | Desugared.Lambda ((p, None), e) ->
     begin
       match ty with
       | Syntax.Arrow (t, u) ->
          let ctx, p = extend_pattern ctx p t in
          let c = check_comp ctx e u in
          locate (Syntax.Lambda (p, c))
       | (Syntax.SignalTy | Syntax.Int | Syntax.Bool | Syntax.Product _ | Syntax.ComodelTy _) ->
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

       | (Syntax.SignalTy | Syntax.Int | Syntax.Bool | Syntax.Arrow _ | Syntax.ComodelTy _) ->
          begin match es with
          | [] -> error ~loc (TypeExpectedButUnit ty)
          | _::_ -> error ~loc (TypeExpectedButTuple ty)
          end
     end

  | (Desugared.Numeral _ | Desugared.Boolean _ | Desugared.Lambda ((_, Some _), _) |
     Desugared.Var _ | Desugared.AscribeExpr _ | Desugared.Comodel _) ->
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

  | (Desugared.Apply _ | Desugared.AscribeComp _ | Desugared.Operation _ |
     Desugared.Signal _ | Desugared.Using _) ->
     let c, c_ty = infer_comp ctx c in
     if Syntax.comp_subty c_ty check_ty
     then
       c
     else
       error ~loc (CompTypeMismatch (check_ty, c_ty))

and check_match_clauses ctx patt_ty ty lst =
  List.map (check_match_clause ctx patt_ty ty) lst

and check_match_clause ctx patt_ty ty (p, c) =
  let ctx, p = extend_binder ctx p patt_ty in
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
       ctx, Syntax.TopComp (c, c_ty')

    | Desugared.DeclOperation (op, ty1, ty2) ->
       let ty1 = expr_ty ty1
       and ty2 = expr_ty ty2 in
       let ctx = declare_operation op ty1 ty2 ctx in
       ctx, Syntax.DeclOperation (op, ty1, ty2)

    | Desugared.DeclSignal (sgl, ty) ->
       let ty = expr_ty ty in
       let ctx = declare_signal sgl ty ctx in
       ctx, Syntax.DeclSignal (sgl, ty)

    | Desugared.External (x, t, s) ->
       let t = expr_ty t in
       let ctx = extend_ident x t ctx in
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
