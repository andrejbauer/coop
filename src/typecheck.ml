(** Coop type checking. *)

(** Typing context *)
type context =
  { ctx_operations : (Syntax.expr_ty * Syntax.expr_ty) Name.Map.t
  ; ctx_signals : Syntax.expr_ty Name.Map.t
  ; ctx_idents : (Name.t * Syntax.expr_ty) list
  ; ctx_tyabbrevs : Syntax.expr_ty Name.Map.t
  ; ctx_datatypes : (Name.t * (Name.t * Syntax.expr_ty option) list) list
  }

(** Initial typing context *)
let initial =
  { ctx_operations = Name.Map.empty
  ; ctx_signals = Name.Map.empty
  ; ctx_idents = []
  ; ctx_tyabbrevs = Name.Map.empty
  ; ctx_datatypes = []
  }

(** Type errors *)
type error =
  | InvalidName of Name.t
  | InvalidOperation of Name.t
  | InvalidSignal of Name.t
  | InvalidType of Name.t
  | PattTypeMismatch of Syntax.expr_ty
  | ExprTypeMismatch of Syntax.expr_ty * Syntax.expr_ty
  | CoopTypeMismatch of Syntax.expr_ty * Syntax.expr_ty
  | ExprTypeExpected
  | WrongTupleLength of int * int
  | IllegalConstructor of Name.t
  | UnknownConstructor of Name.t
  | WrongConstructor of Name.t * Syntax.expr_ty
  | CompTypeMismatch of Syntax.comp_ty * Syntax.comp_ty
  | TypeExpectedButFunction of Syntax.expr_ty
  | TypeExpectedButTuple of Syntax.expr_ty
  | TypeExpectedButUnit of Syntax.expr_ty
  | TypeExpectedButConstructor of Syntax.expr_ty
  | FunctionExpected of Syntax.expr_ty
  | ComodelExpected of Syntax.expr_ty
  | ComodelDoubleOperations of Name.Set.t
  | CannotInferArgument
  | CannotInferMatch
  | DuplicateOperation of Name.t
  | DuplicateSignal of Name.t
  | UnhandledOperationsSignals of Name.Set.t * Name.Set.t
  | RenamingMismatch of Name.t * Name.t
  | RenamingDomain of Name.t

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

  | InvalidType ty ->
     Format.fprintf ppf "invalid ty %t, please report"
       (Name.print ty)

  | PattTypeMismatch ty_expected ->
     Format.fprintf ppf "this pattern should have type@ %t"
       (Syntax.print_expr_ty ty_expected)

  | ExprTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this expression should have type@ %t but has type@ %t"
       (Syntax.print_expr_ty ty_expected)
       (Syntax.print_expr_ty ty_actual)

  | CoopTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this co-operation should return values of type@ %t but it returns values of type@ %t"
       (Syntax.print_expr_ty ty_expected)
       (Syntax.print_expr_ty ty_actual)

  | CompTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this computation should have type@ %t but has type@ %t"
       (Syntax.print_comp_ty ty_expected)
       (Syntax.print_comp_ty ty_actual)

  | ExprTypeExpected ->
     Format.fprintf ppf "this type should be pure"

  | WrongTupleLength (k_expected, k_actual) ->
     Format.fprintf ppf "this tuple should have %d component but has %d" k_expected k_actual

  | IllegalConstructor cnstr ->
     Format.fprintf ppf "illegal application of constructor %t, please report"
       (Name.print cnstr)

  | UnknownConstructor cnstr ->
     Format.fprintf ppf "unknown constructor %t" (Name.print cnstr)

  | WrongConstructor (cnstr, ty) ->
     Format.fprintf ppf "constructor %t does not have type %t"
       (Name.print cnstr)
       (Syntax.print_expr_ty ty)

  | TypeExpectedButFunction ty ->
     Format.fprintf ppf "this expression is a function but should have type@ %t"
                        (Syntax.print_expr_ty ty)

  | TypeExpectedButTuple ty ->
     Format.fprintf ppf "this expression is a tuple but should have type@ %t"
       (Syntax.print_expr_ty ty)

  | TypeExpectedButUnit ty ->
     Format.fprintf ppf "this expression is the unit but should have type@ %t"
       (Syntax.print_expr_ty ty)

  | TypeExpectedButConstructor ty ->
     Format.fprintf ppf "this expression is a constructor but should have type@ %t"
       (Syntax.print_expr_ty ty)

  | FunctionExpected ty ->
     Format.fprintf ppf "this expression should be a function but has type@ %t"
       (Syntax.print_expr_ty ty)

  | ComodelExpected ty ->
     Format.fprintf ppf "this expression should be a comodel but has type@ %t"
       (Syntax.print_expr_ty ty)

  | ComodelDoubleOperations ops ->
     let ops = Name.Set.elements ops in
     Format.fprintf ppf "these comodels both handle the following operations:@ %t"
       (Print.sequence (Name.print ~parentheses:true) "," ops)

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

  | RenamingMismatch (op, op') ->
     Format.fprintf ppf "cannot rename %t to %t, they have different types"
       (Name.print op)
       (Name.print op')

  | RenamingDomain op ->
     Format.fprintf ppf "operation %t is not there and cannot be renamed"
       (Name.print op)

(** Extend the context with the type of deBruijn index 0 *)
let extend_ident x ty ctx =
  { ctx with ctx_idents = (x, ty) :: ctx.ctx_idents }

let rec extend_idents xts ctx =
  match xts with
  | [] -> ctx
  | (x, t) :: xts -> extend_idents xts (extend_ident x t ctx)

let extend_tyabbrev ~loc x ty ctx =
  { ctx with ctx_tyabbrevs = Name.Map.add x ty ctx.ctx_tyabbrevs }

let extend_datatype ~loc x cnstrs ctx =
  { ctx with ctx_datatypes = (x, cnstrs) :: ctx.ctx_datatypes }

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

let lookup_tyabbrev ~loc ty {ctx_tyabbrevs;_} =
  match Name.Map.find ty ctx_tyabbrevs with
  | None -> error ~loc (InvalidType ty)
  | Some abbrev -> abbrev

let lookup_datatype ~loc ty {ctx_datatypes;_} =
  match List.assoc_opt ty ctx_datatypes with
  | None -> error ~loc (InvalidType ty)
  | Some def -> def

let lookup_constructor ~loc cnstr {ctx_datatypes;_} =
  let rec find = function

    | [] ->
       error ~loc (UnknownConstructor cnstr)

    | (ty, cnstrs) :: lst ->
       begin
         match List.assoc_opt cnstr cnstrs  with

         | Some ts -> (ty, ts)

         | None -> find lst
       end
  in
  find ctx_datatypes

let signature Desugared.{sig_ops; sig_sgs} = Syntax.{sig_ops; sig_sgs}

(**** Normalization of types ****)

(** Unfold the definition of a type *)
let rec norm_ty ~loc ctx t =
  match t with

  | Syntax.Alias x ->
     let t = lookup_tyabbrev ~loc x ctx in
     norm_ty ~loc ctx t

  | (Syntax.Datatype _ |  Syntax.Empty | Syntax.Int | Syntax.Bool |
     Syntax.Product _ | Syntax.Arrow _ | Syntax.ComodelTy _) ->
     t

let as_product ~loc ctx ty =
  match norm_ty ~loc ctx ty with

  | Syntax.Alias _ -> assert false

  | Syntax.Product ts -> Some ts

  | (Syntax.Datatype _  | Syntax.Empty | Syntax.Int | Syntax.Bool |
     Syntax.Arrow _ | Syntax.ComodelTy _) ->
     None

let as_arrow ~loc ctx ty =
  match norm_ty ~loc ctx ty with

  | Syntax.Alias _ -> assert false

  | Syntax.Arrow (t,u) -> Some (t, u)

  | (Syntax.Product _ | Syntax.Datatype _ | Syntax.Empty | Syntax.Int | Syntax.Bool |
     Syntax.ComodelTy _) ->
     None

let as_comodel ~loc ctx ty =

  match norm_ty ~loc ctx ty with

  | Syntax.Alias _ -> assert false

  | Syntax.ComodelTy (ops, w_ty, sgn) -> Some (ops, w_ty, sgn)

  | (Syntax.Product _ | Syntax.Datatype _ | Syntax.Empty | Syntax.Int | Syntax.Bool | Syntax.Arrow _) ->
     None

let as_datatype ~loc ctx ty =

  match norm_ty ~loc ctx ty with

  | Syntax.Alias _ -> assert false

  | Syntax.Datatype ty ->
     Some (lookup_datatype ~loc ty ctx)

  | (Syntax.Product _ | Syntax.Empty | Syntax.Int | Syntax.Bool | Syntax.Arrow _ | Syntax.ComodelTy _) ->
     None


(**** Subtyping ****)

(** Is the first signature a subsignature of the second one? *)
let subsignature Syntax.{sig_ops=ops1; sig_sgs=sgs1} Syntax.{sig_ops=ops2; sig_sgs=sgs2} =
  Name.Set.subset ops1 ops2 && Name.Set.subset sgs1 sgs2

let rec expr_subty ~loc ctx t u =
  match t, u with

  | Syntax.Alias x, Syntax.Alias y when Name.equal x y -> true

  | Syntax.Alias _, _ ->
     let t = norm_ty ~loc ctx t in
     expr_subty ~loc ctx t u

  | _, Syntax.Alias _ ->
     let u = norm_ty ~loc ctx u in
     expr_subty ~loc ctx t u

  | Syntax.Datatype x, Syntax.Datatype y ->
     Name.equal x y

  | Syntax.Empty, _ -> true

  | _, Syntax.Empty -> false

  | Syntax.Int, Syntax.Int -> true

  | Syntax.Bool, Syntax.Bool -> true

  | Syntax.Product ts, Syntax.Product us ->
     let rec fold ts us =
       match ts, us with
       | [], [] -> true
       | t :: ts, u :: us -> expr_subty ~loc ctx t u && fold ts us
       | [], _::_ | _::_, [] -> false
     in
     fold ts us

  | Syntax.Arrow (t1, t2), Syntax.Arrow (u1, u2) ->
     expr_subty ~loc ctx u1 t1 && comp_subty ~loc ctx t2 u2

  | Syntax.ComodelTy (tsgn1, t, tsgn2), Syntax.ComodelTy (usgn1, u, usgn2) ->
     Name.Set.subset usgn1 tsgn1 && expr_eqtype ~loc ctx t u && subsignature  tsgn2 usgn2

  | (Syntax.Datatype _ | Syntax.Int | Syntax.Bool | Syntax.Product _ |
     Syntax.Arrow _ | Syntax.ComodelTy _), _ ->
     false

and comp_subty ~loc ctx Syntax.{comp_ty=t1; comp_sig=sig1} (Syntax.{comp_ty=t2; comp_sig=sig2}) =
  subsignature sig1 sig2 && expr_subty ~loc ctx t1 t2

and expr_eqtype ~loc ctx t u =
  expr_subty ~loc ctx t u && expr_subty ~loc ctx u t

let join_signature Syntax.{sig_ops=ops1; sig_sgs=sgs1} Syntax.{sig_ops=ops2; sig_sgs=sgs2} =
  let sig_ops = Name.Set.union ops1 ops2
  and sig_sgs = Name.Set.union sgs1 sgs2 in
  Syntax.{sig_ops; sig_sgs}

let meet_signature Syntax.{sig_ops=ops1; sig_sgs=sgs1} Syntax.{sig_ops=ops2; sig_sgs=sgs2} =
  let sig_ops = Name.Set.inter ops1 ops2
  and sig_sgs = Name.Set.inter sgs1 sgs2 in
  Syntax.{sig_ops; sig_sgs}

let rec join_expr_ty ~loc ctx t1 t2 =
  match t1, t2 with

  | Syntax.Alias x, Syntax.Alias y when Name.equal x y ->
     t1

  | Syntax.Alias _, _ ->
     let t1 = norm_ty ~loc ctx t1 in
     join_expr_ty ~loc ctx t1 t2

  | _, Syntax.Alias _ ->
     let t2 = norm_ty ~loc ctx t2 in
     join_expr_ty ~loc ctx t1 t2

  | Syntax.Datatype x, Syntax.Datatype y when Name.equal x y ->
     t1

  | Syntax.Empty, t2 -> t2

  | t1, Syntax.Empty -> t1

  | Syntax.Int, Syntax.Int -> Syntax.Int

  | Syntax.Bool, Syntax.Bool -> Syntax.Bool

  | Syntax.Product ts1, Syntax.Product ts2 ->
     let rec fold ts ts1 ts2 =
       match ts1, ts2 with
       | [], [] ->
          let ts = List.rev ts in
          Syntax.Product ts
       | t1::ts1, t2::ts2 ->
          let t = join_expr_ty ~loc ctx t1 t2 in
          fold (t :: ts) ts1 ts2
       | [], _::_ | _::_, [] ->
          error ~loc (ExprTypeMismatch (t2, t1))
     in
     fold [] ts1 ts2

  | Syntax.Arrow (u1, t1), Syntax.Arrow (u2, t2) ->
     let u = meet_expr_ty ~loc ctx u1 u2 in
     let t = join_comp_ty ~loc ctx t1 t2 in
     Syntax.Arrow (u, t)

  | Syntax.ComodelTy (ops1, t1, sig1), Syntax.ComodelTy (ops2, t2, sig2) ->
     let ops = Name.Set.inter ops1 ops2
     and t = meet_expr_ty ~loc ctx t1 t2
     and sgn = join_signature sig1 sig2 in
     Syntax.ComodelTy  (ops, t, sgn)

  | (Syntax.Datatype _ | Syntax.Int | Syntax.Bool | Syntax.Product _ |
     Syntax.Arrow _ | Syntax.ComodelTy _), _ ->
     error ~loc (ExprTypeMismatch (t2, t2))

and meet_expr_ty ~loc ctx t1 t2 =
  match t1, t2 with

  | Syntax.Alias x, Syntax.Alias y when Name.equal x y ->
     t1

  | Syntax.Alias _, _ ->
     let t1 = norm_ty ~loc ctx t1 in
     meet_expr_ty ~loc ctx t1 t2

  | _, Syntax.Alias _ ->
     let t2 = norm_ty ~loc ctx t2 in
     meet_expr_ty ~loc ctx t1 t2

  | Syntax.Datatype x, Syntax.Datatype y when Name.equal x y ->
     t1

  | Syntax.Empty, _ -> Syntax.Empty

  | _, Syntax.Empty -> Syntax.Empty

  | Syntax.Int, Syntax.Int -> Syntax.Int

  | Syntax.Bool, Syntax.Bool -> Syntax.Bool

  | Syntax.Product ts1, Syntax.Product ts2 ->
     let rec fold ts ts1 ts2 =
       match ts1, ts2 with
       | [], [] ->
          let ts = List.rev ts in
          Syntax.Product ts
       | t1::ts1, t2::ts2 ->
          let t = meet_expr_ty ~loc ctx t1 t2 in
          fold (t :: ts) ts1 ts2
       | [], _::_ | _::_, [] ->
          error ~loc (ExprTypeMismatch (t2, t1))
     in
     fold [] ts1 ts2

  | Syntax.Arrow (u1, t1), Syntax.Arrow (u2, t2) ->
     let u = join_expr_ty ~loc ctx u1 u2 in
     let t = meet_comp_ty ~loc ctx t1 t2 in
     Syntax.Arrow (u, t)

  | Syntax.ComodelTy (ops1, t1, sig1), Syntax.ComodelTy (ops2, t2, sig2) ->
     let ops = Name.Set.union ops1 ops2
     and t = join_expr_ty ~loc ctx t1 t2
     and sgn = meet_signature sig1 sig2 in
     Syntax.ComodelTy  (ops, t, sgn)

  | (Syntax.Datatype _ | Syntax.Int | Syntax.Bool | Syntax.Product _ |
     Syntax.Arrow _ | Syntax.ComodelTy _), _ ->
     error ~loc (ExprTypeMismatch (t2, t2))

and join_comp_ty ~loc ctx Syntax.{comp_ty=t1; comp_sig=sig1} Syntax.{comp_ty=t2; comp_sig=sig2} =
  let t = join_expr_ty ~loc ctx t1 t2
  and sgn = join_signature sig1 sig2 in
  Syntax.{comp_ty=t; comp_sig=sgn}

and meet_comp_ty ~loc ctx Syntax.{comp_ty=t1; comp_sig=sig1} Syntax.{comp_ty=t2; comp_sig=sig2} =
  let t = meet_expr_ty ~loc ctx t1 t2
  and sgn = meet_signature sig1 sig2 in
  Syntax.{comp_ty=t; comp_sig=sgn}

(**** Type checking ****)

(** Check that a type is valid. Retrn the processed type. *)
let rec expr_ty {Location.it=t'; loc} =
  match t' with

  | Desugared.Int -> Syntax.Int

  | Desugared.Bool -> Syntax.Bool

  | Desugared.Alias t -> Syntax.Alias t

  | Desugared.Datatype t -> Syntax.Datatype t

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

and comp_ty ({Location.it=t'; loc} as t) =
  match t' with

  | Desugared.CompTy (t, sgn) ->
     let t = expr_ty t
     and sgn = signature sgn in
     Syntax.{comp_ty=t; comp_sig=sgn}

  | (Desugared.Int | Desugared.Bool | Desugared.Alias _  | Desugared.Datatype _ |
     Desugared.Product _ | Desugared.Arrow _ | Desugared.ComodelTy _) ->
     let t = expr_ty t in
     Syntax.{comp_ty=t; comp_sig=empty_signature}


(** Typecheck a pattern, return processed pattern and the list of identifiers
   and types bound by the pattern *)
let check_pattern ~loc ctx patt ty =
  let rec fold xts {Location.it=p'; loc} t =
    let t = norm_ty ~loc ctx t in
    match p', t with

    | _, Syntax.Alias _ -> assert false

    | Desugared.PattAnonymous, _ ->
       Syntax.PattAnonymous, xts

    | Desugared.PattVar x, _ ->
       Syntax.PattVar, (x, t) :: xts

    | Desugared.PattNumeral n, Syntax.Int ->
       Syntax.PattNumeral n, xts

    | Desugared.PattBoolean b, Syntax.Bool ->
       Syntax.PattBoolean b, xts

    | Desugared.PattConstructor (cnstr, popt), Syntax.Datatype x ->
       let lst = lookup_datatype ~loc x ctx in
       begin match List.assoc_opt cnstr lst with
         | None -> error ~loc (PattTypeMismatch ty)
         | Some topt ->
            begin
              match popt, topt with
              | None, None ->
                 Syntax.PattConstructor (cnstr, None), xts
              | Some p, Some t ->
                 let p, xts = fold xts p t in
                 Syntax.PattConstructor (cnstr, Some p), xts
              | None, Some _ | Some _, None -> error ~loc  (PattTypeMismatch ty)
            end
       end

    | Desugared.PattTuple ps, Syntax.Product ts ->
       let ps, xts = fold_tuple ~loc xts [] ps ts in
       Syntax.PattTuple ps, xts

    | Desugared.PattNumeral _, (Syntax.Empty | Syntax.Bool | Syntax.Datatype _ | Syntax.Product _ | Syntax.Arrow _ | Syntax.ComodelTy _)
    | Desugared.PattBoolean _, (Syntax.Empty | Syntax.Int | Syntax.Datatype _ | Syntax.Product _ | Syntax.Arrow _ | Syntax.ComodelTy _)
    | Desugared.PattConstructor  _, (Syntax.Empty | Syntax.Int | Syntax.Bool | Syntax.Product _ | Syntax.Arrow _ | Syntax.ComodelTy _)
    | Desugared.PattTuple _, (Syntax.Empty | Syntax.Int | Syntax.Bool | Syntax.Datatype _ | Syntax.Arrow _ | Syntax.ComodelTy _) ->
       error ~loc (PattTypeMismatch ty)

  and fold_tuple ~loc xts ps' ps ts =
    match ps, ts with
    | [], [] ->
       let ps' = List.rev ps' in
       ps', xts
    | p :: ps, t :: ts ->
       let p', xts = fold xts p t in
       fold_tuple ~loc xts (p' :: ps') ps ts
    | ([], _::_ | _::_, []) ->
       error ~loc (PattTypeMismatch (Syntax.Product ts))
  in

  let p, xts = fold [] patt ty in
  p, List.rev xts

(** Extend the context by typechecking the pattern against the type *)
let extend_pattern ~loc ctx p t =
  let p, xts = check_pattern ~loc ctx p t in
  let ctx = extend_idents xts ctx in
  ctx, p

(** Extend the context by typechecking the pattern against the type,
    also return the list of bound names with their types. *)
let top_extend_pattern ~loc ctx p t =
  let p, xts = check_pattern ~loc ctx p t in
  let ctx = extend_idents xts ctx in
  ctx, p, xts

(** Make sure that the dirt of the first type is a subsignature of [sgn], or report an error. *)
let check_dirt ~loc
  Syntax.{comp_sig={sig_ops=ops1; sig_sgs=sgs1}; _}
  Syntax.{sig_ops=ops2; sig_sgs=sgs2} =
  let ops = Name.Set.diff ops1 ops2 in
  let sgs = Name.Set.diff sgs1 sgs2 in
  if not (Name.Set.is_empty ops && Name.Set.is_empty sgs) then
    error ~loc (UnhandledOperationsSignals (ops, sgs))


(** [infer_expr ctx e] infers the expression type [ty] of an expression [e]. It
   returns the processed expression [e] and its type [ty]. *)
let rec infer_expr (ctx : context) {Location.it=e'; loc} =
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

  | Desugared.Constructor (cnstr, eopt) ->
     let ty, topt = lookup_constructor ~loc cnstr ctx in
     let e = check_constructor ~loc ctx cnstr eopt topt in
     locate (Syntax.Constructor (cnstr, e)), Syntax.Datatype ty

  | Desugared.Tuple lst ->
     let lst = List.map (infer_expr ctx) lst in
     locate (Syntax.Tuple (List.map fst lst)),  Syntax.Product (List.map snd lst)

  | Desugared.Lambda ((p, Some t), c) ->
     let t = expr_ty t in
     let ctx, p = extend_pattern ~loc ctx p t in
     let c, c_ty = infer_comp ctx c in
     locate (Syntax.Lambda (p, c)), Syntax.Arrow (t, c_ty)

  | Desugared.Lambda ((p, None), _) ->
     error ~loc:p.Location.loc CannotInferArgument

  | Desugared.Comodel (e, coops) ->
     let e, e_ty = infer_expr ctx e in
     let coops, ops, sgn = infer_coops ~loc ctx e_ty coops in
     locate (Syntax.Comodel (e, coops)), Syntax.ComodelTy (ops, e_ty, sgn)

  | Desugared.ComodelTimes (e1, e2) ->
     let cmdl1, (ops1, w1_ty, sgn1) = infer_comodel ctx e1
     and cmdl2, (ops2, w2_ty, sgn2) = infer_comodel ctx e2 in
     let w_ty = Syntax.Product [w1_ty; w2_ty] in
     let ops' = Name.Set.inter ops1 ops2 in
     if not (Name.Set.is_empty ops') then
       error ~loc (ComodelDoubleOperations ops') ;
     let ops = Name.Set.union ops1 ops2 in
     let sgn = join_signature sgn1 sgn2 in
     locate (Syntax.ComodelTimes (cmdl1, cmdl2)), Syntax.ComodelTy (ops, w_ty, sgn)

  | Desugared.ComodelRename (e, rnm) ->
     let e, (ops, w_ty, sgn) = infer_comodel ctx e in
     begin
       match List.find_opt (fun (op, _) -> not (Name.Set.mem op ops)) rnm with
       | None -> ()
       | Some (op, _) -> error ~loc (RenamingDomain op)
     end ;
     let ops', rnm' =
       Name.Set.fold
         (fun op (ops', rnm') ->
           let (ty1, ty2) = lookup_operation ~loc op ctx in
           let op' =
             match List.assoc_opt op rnm with
             | None -> op
             | Some op' ->
                let (ty1', ty2') = lookup_operation ~loc op' ctx in
                if not (expr_eqtype ~loc ctx ty1 ty1' && expr_eqtype ~loc ctx ty2 ty2') then
                  error ~loc (RenamingMismatch (op, op')) ;
                op'
           in
           if Name.Set.mem op' ops' then error ~loc (DuplicateOperation op') ;
           Name.Set.add op' ops', Name.Map.add op op' rnm')
         ops
         (Name.Set.empty, Name.Map.empty)
     in
     locate (Syntax.ComodelRename (e, rnm')), Syntax.ComodelTy (ops', w_ty, sgn)


(** [infer_comp ctx c] infers the type [ty] of a computation [c]. It returns
    the processed computation [c] and its type [ty].  *)
and infer_comp (ctx : context) {Location.it=c'; loc} =
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
     let c1, Syntax.{comp_ty=c1_ty; comp_sig=c1_sgn} = infer_comp ctx c1 in
     let ctx, p = extend_pattern ~loc ctx p c1_ty in
     let c2, c2_ty = infer_comp ctx c2 in
     let c2_ty = Syntax.pollute c2_ty c1_sgn in
     locate (Syntax.Let (p, c1, c2)), c2_ty

  | Desugared.LetRec (fs, c) ->
     let ctx, pcs, fts = infer_rec ~loc ctx fs in
     let c, c_ty = infer_comp ctx c in
     locate (Syntax.LetRec (pcs, c)), c_ty

  | Desugared.Match (e, lst) ->
     let e, e_ty = infer_expr ctx e in
     let lst, ty = infer_match_clauses ~loc ctx e_ty lst in
     locate (Syntax.Match (e, lst)), ty

  | Desugared.Apply (e1, e2) ->
     let e1, t1 = infer_expr ctx e1 in
     begin match as_arrow ~loc:(e1.Location.loc) ctx t1 with
       | Some (u1, u2) ->
          let e2 = check_expr ctx e2 u1 in
          locate (Syntax.Apply (e1, e2)), u2
       | None ->
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

  | Desugared.Using (cmdl, c, fin) ->
     let cmdl, (ops, w_ty, Syntax.{sig_ops=cmdl_ops; sig_sgs=cmdl_sgs}) =
       infer_comodel ctx cmdl in
     let c, (Syntax.{comp_ty=x_ty; _} as c_ty) = infer_comp ctx c in
     let fin, fin_sgs, fin_ty = infer_finally ~loc ctx x_ty w_ty fin in
     let cmdl_sig = Syntax.{sig_ops=cmdl_ops; sig_sgs = Name.Set.diff cmdl_sgs fin_sgs} in
     let fin_ty = Syntax.pollute fin_ty cmdl_sig in
     check_dirt ~loc c_ty Syntax.{sig_ops=ops; sig_sgs=fin_sgs} ;
     locate (Syntax.Using (cmdl, c, fin)), fin_ty

and infer_rec ~loc ctx fs =
  let ctx, fts =
    List.fold_left
      (fun (ctx, fts) (f, t, _, u, _) ->
        let t = comp_ty t
        and u = expr_ty u in
        extend_ident f (Syntax.Arrow (u, t)) ctx,
        (f, u, t) :: fts)
      (ctx, [])
      fs
  in
  let pcs =
    List.map2
    (fun (_, _, p, _, c) (_, u, t) ->
      let ctx, p = extend_pattern ~loc:p.Location.loc ctx p u in
      let c = check_comp ctx c t in
      (p, c))
    fs fts
  in
  ctx, pcs, fts

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
          let ty = join_comp_ty ~loc:c.Location.loc ctx c_ty ty in
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
         let c, (Syntax.{comp_sig=c_sgn;comp_ty=c_ty'} as c_ty) = infer_comp ctx c in
         let c_ty'' = Syntax.Product [op_ty; w_ty] in
         if not (expr_subty ~loc ctx c_ty' c_ty'') then
           error ~loc (CoopTypeMismatch (c_ty'', c_ty')) ;
         let coops = (op, px, pw, c) :: coops
         and ops = Name.Set.add op ops
         and sgn2 = join_signature sgn2 c_sgn in
         fold coops ops sgn2 lst
  in
  fold [] Name.Set.empty Syntax.empty_signature lst

and infer_comodel ctx cmdl =
  let e, e_ty = infer_expr ctx cmdl in
  match as_comodel ~loc:cmdl.Location.loc ctx e_ty with

    | Some (ops, t, sgn2) ->
       e, (ops, t, sgn2)

    | None ->
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
           let ty = join_comp_ty ~loc:c_sg.Location.loc ctx ty sg_ty in
           let sgs = Name.Set.add sg sgs in
           fold ((sg, px, pw, c_sg) :: fs) sgs ty lst
         end
    in
    fold [] Name.Set.empty c_ty fin_signals
  in
  Syntax.{fin_val; fin_signals}, fin_sgs, fin_ty


and extend_binder ctx (p, topt) t =
  let loc = p.Location.loc in
  match topt with
  | None -> extend_pattern ~loc ctx p t
  | Some t' ->
     let t' = expr_ty t' in
     if not (expr_subty ~loc ctx t t') then
       error ~loc (ExprTypeMismatch (t, t')) ;
     extend_pattern ~loc ctx p t'

(** [check_expr ctx e ty] checks that expression [e] has type [ty] in context [ctx].
    It returns the processed expression [e]. *)
and check_expr (ctx : context) ({Location.it=e'; loc} as e) ty =
  let locate = Location.locate ~loc in
  match e' with

  | Desugared.Lambda ((p, None), e) ->
     begin
       match as_arrow ~loc ctx ty with
       | Some (t, u) ->
          let ctx, p = extend_pattern ~loc ctx p t in
          let c = check_comp ctx e u in
          locate (Syntax.Lambda (p, c))
       | None ->
          error ~loc (TypeExpectedButFunction ty)
     end

  | Desugared.Constructor (cnstr, eopt) ->
     begin
       match as_datatype ~loc ctx ty with

       | None ->
          error ~loc (TypeExpectedButConstructor ty)

       | Some cnstrs ->
          begin
            match List.assoc_opt cnstr cnstrs with

            | None -> error ~loc (WrongConstructor (cnstr, ty))

            | Some topt ->
               let e = check_constructor ~loc ctx cnstr eopt topt in
               locate (Syntax.Constructor (cnstr, e))
          end
     end

  | Desugared.Tuple es ->
     begin
       match as_product ~loc ctx ty with
       | Some ts ->
          let k_actual = List.length es
          and k_expected = List.length ts in
          if k_expected <> k_actual then error ~loc (WrongTupleLength (k_expected, k_actual)) ;
          let es = check_tuple ctx es ts in
          locate (Syntax.Tuple es)
       | None ->
          if List.length es = 0 then
            error ~loc (TypeExpectedButUnit ty)
          else
            error ~loc (TypeExpectedButTuple ty)
     end

  | (Desugared.Numeral _ | Desugared.Boolean _ | Desugared.Lambda ((_, Some _), _) |
     Desugared.Var _ | Desugared.AscribeExpr _ | Desugared.Comodel _ |
     Desugared.ComodelTimes _ | Desugared.ComodelRename _) ->
     let e, ty' = infer_expr ctx e in
     if expr_subty ~loc ctx ty' ty
     then
       e
     else
       error ~loc (ExprTypeMismatch (ty, ty'))

and check_tuple ctx es ts =
  let rec fold es ts es' =
    match es, ts with
    | [], [] ->
       List.rev es'
    | e :: es, t :: ts ->
       let e' = check_expr ctx e t in
       fold es ts (e' :: es')
    | _ :: _, [] | [], _ :: _ -> assert false
  in
  fold es ts []


and check_constructor ~loc ctx cnstr eopt topt =
  match eopt, topt with

  | None, None -> None

  | Some e, Some t ->
     let e = check_expr ctx e t in
     Some e

  | None, Some t ->
     error ~loc (IllegalConstructor cnstr)

  | Some e, None ->
     error ~loc (IllegalConstructor cnstr)

(** [check_comp ctx c ty] checks that computation [c] has computation type [ty] in context [ctx].
    It returns the processed computation [c]. *)
and check_comp ctx ({Location.it=c'; loc} as c) check_ty =
  let Syntax.{comp_ty=check_ty'; comp_sig=check_sgn} = check_ty in
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
     let c1, (Syntax.{comp_ty=c1_ty';_} as c1_ty) = infer_comp ctx c1 in
     check_dirt ~loc c1_ty check_sgn ;
     let ctx, p = extend_pattern ~loc ctx p c1_ty' in
     let c2 = check_comp ctx c2 check_ty in
     locate (Syntax.Let (p, c1, c2))

  | Desugared.LetRec (fs, c) ->
     let ctx, pcs, fts = infer_rec ~loc ctx fs in
     let c = check_comp ctx c check_ty in
     locate (Syntax.LetRec (pcs, c))

  | (Desugared.Apply _ | Desugared.AscribeComp _ | Desugared.Operation _ |
     Desugared.Signal _ | Desugared.Using _) ->
     let c, c_ty = infer_comp ctx c in
     if comp_subty ~loc ctx c_ty check_ty
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

and datatype cnstrs =
  List.map
    (function
     | (x, None) -> (x, None)
     | (x, Some t) -> (x, Some (expr_ty t)))
    cnstrs

and datatypes ~loc ctx ty_defs =
  let rec fold ctx ty_defs = function

    | [] ->
       let ty_defs = List.rev ty_defs in
       ctx, ty_defs

    | (t, cnstrs) :: lst ->
       let cnstrs = datatype cnstrs in
       let ctx = extend_datatype ~loc t cnstrs ctx in
       fold ctx ((t, cnstrs) :: ty_defs) lst
  in
  fold ctx [] ty_defs

and toplevel ~quiet ctx {Location.it=d'; loc} =
  let ctx, d' =
    match d' with

    | Desugared.TopLoad lst ->
       let ctx, lst = topfile ~quiet ctx lst in
       ctx, Syntax.TopLoad lst

    | Desugared.TopLet (p, c) ->
       let c, (Syntax.{comp_ty=c_ty';_} as c_ty) = infer_comp ctx c in
       check_dirt ~loc c_ty Syntax.empty_signature ;
       let ctx, p, xts = top_extend_pattern ~loc ctx p c_ty' in
       ctx, Syntax.TopLet (p, xts, c)

    | Desugared.TopLetRec fs ->
       let ctx, pcs, fts = infer_rec ~loc ctx fs in
       let fts = List.map (fun (f, u, t) -> (f, Syntax.Arrow (u, t))) fts in
       ctx, Syntax.TopLetRec (pcs, fts)

    | Desugared.TopComp c ->
       let c, Syntax.{comp_ty=c_ty'; _} = infer_comp ctx c in
       ctx, Syntax.TopComp (c, c_ty')

    | Desugared.DefineAlias (t, abbrev) ->
       let abbrev = expr_ty abbrev in
       let ctx = extend_tyabbrev ~loc t abbrev ctx in
       ctx, Syntax.DefineAlias (t, abbrev)

    | Desugared.DefineDatatype ty_defs ->
       let ctx, ty_defs = datatypes ~loc ctx ty_defs in
       ctx, Syntax.DefineDatatype ty_defs

    | Desugared.DeclareOperation (op, ty1, ty2) ->
       let ty1 = expr_ty ty1
       and ty2 = expr_ty ty2 in
       let ctx = declare_operation op ty1 ty2 ctx in
       ctx, Syntax.DeclareOperation (op, ty1, ty2)

    | Desugared.DeclareSignal (sgl, ty) ->
       let ty = expr_ty ty in
       let ctx = declare_signal sgl ty ctx in
       ctx, Syntax.DeclareSignal (sgl, ty)

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
