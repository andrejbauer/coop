(** Desugaring errors *)
type desugar_error =
  | UnknownIdentifier of Name.t
  | UnknownOperation of Name.t
  | UnknownConstructor of Name.t
  | UnknownType of Name.t
  | SignalExpected of Name.t
  | OperationExpected of Name.t
  | OperationOrSignalExpected of Name.t
  | ShadowOperation of Name.t
  | ShadowSignal of Name.t
  | ShadowConstructor of Name.t
  | ConstructorCannotApply of Name.t
  | ConstructorMustApply of Name.t
  | ShadowType of Name.t
  | BareOperation
  | BareSignal
  | MissingFinallyVal
  | DoubleFinallyVal
  | DoubleFinallySignal of Name.t

(** The exception signalling a desugaring error*)
exception Error of desugar_error Location.located

(** [error ~loc err] raises the given desugaring error. *)
let error ~loc err = Stdlib.raise (Error (Location.locate ~loc err))

(** Print desugaring error. *)
let print_error err ppf =
  match err with

  | UnknownIdentifier x ->
     Format.fprintf ppf "unknown identifier %t" (Name.print x)

  | UnknownOperation op ->
     Format.fprintf ppf "unknown operation %t" (Name.print op)

  | UnknownConstructor cnstr ->
     Format.fprintf ppf "unknown constructor %t" (Name.print cnstr)

  | UnknownType ty ->
     Format.fprintf ppf "unknown type %t" (Name.print ty)

  | SignalExpected x ->
     Format.fprintf ppf "%t is not a signal" (Name.print x)

  | OperationExpected x ->
     Format.fprintf ppf "%t is not an operation" (Name.print x)

  | OperationOrSignalExpected x ->
     Format.fprintf ppf "%t is neither an operation nor a signal" (Name.print x)

  | ShadowOperation op ->
     Format.fprintf ppf "%t is already declared to be an operation" (Name.print op)

  | ShadowSignal sgl ->
     Format.fprintf ppf "%t is already declared to be a signal" (Name.print sgl)

  | ShadowConstructor cnstr ->
     Format.fprintf ppf "constructor %t is already exists" (Name.print cnstr)

  | ConstructorCannotApply cnstr ->
     Format.fprintf ppf "constructor %t cannot be applied" (Name.print cnstr)

  | ConstructorMustApply cnstr ->
     Format.fprintf ppf "constructor %t should be applied" (Name.print cnstr)

  | ShadowType ty ->
     Format.fprintf ppf "the type %t is already defined" (Name.print ty)

  | BareOperation ->
     Format.fprintf ppf "this operation should be applied to an argument"

  | BareSignal ->
     Format.fprintf ppf "this signal should be applied to an argument"

  | MissingFinallyVal ->
     Format.fprintf ppf "missing finally value clause"

  | DoubleFinallyVal ->
     Format.fprintf ppf "multiple value clauses"

  | DoubleFinallySignal sgl ->
     Format.fprintf ppf "multile signal %t clauses" (Name.print sgl)


(** A desugaring context resolves names to their kinds. *)
type ident_kind =
  | Variable
  | Operation
  | Signal

type tydef_kind =
  | TydefAbstract
  | TydefAlias
  | TydefDatatype

type constructor_kind =
  | ConstrApplied
  | ConstrNonApplied

type context = {
    ctx_idents : ident_kind Name.Map.t ;
    ctx_constructors : constructor_kind Name.Map.t ;
    ctx_types : tydef_kind Name.Map.t
  }

(** Initial empty context *)
let initial =
  { ctx_idents = Name.Map.empty ;
    ctx_constructors = Name.Map.empty ;
    ctx_types = Name.Map.empty
  }

(** Add a new identifier of the given kind to the context. *)
let extend_ident x k ctx =
  { ctx with ctx_idents = Name.Map.add x k ctx.ctx_idents }

let extend_constructor x k ctx =
  { ctx with ctx_constructors = Name.Map.add x k ctx.ctx_constructors }

let extend_type x k ctx =
  { ctx with ctx_types = Name.Map.add x k ctx.ctx_types }

let lookup_ident x {ctx_idents;_} = Name.Map.find x ctx_idents

let lookup_constructor x {ctx_constructors;_} = Name.Map.find x ctx_constructors

let lookup_ty x {ctx_types;_} = Name.Map.find x ctx_types

let is_operation op ctx =
  match lookup_ident op ctx with
  | Some Operation -> true
  | Some (Variable | Signal) | None -> false

let is_signal sgl ctx =
  match lookup_ident sgl ctx with
  | Some Signal -> true
  | Some (Variable | Operation) | None -> false

(** Check whether [x] shadows an operation or a signal *)
let check_ident_shadow ~loc x ctx =
  match lookup_ident x ctx with
  | None | Some Variable -> ()
  | Some Operation -> error ~loc (ShadowOperation x)
  | Some Signal -> error ~loc (ShadowSignal x)

(** Check whether [x] shadows a type *)
let check_type_shadow ~loc x ctx =
  match lookup_ty x ctx with
  | None -> ()
  | Some _ -> error ~loc (ShadowType x)

(** Check whether [x] shadows a constructor *)
let check_constructor_shadow ~loc x ctx =
  match lookup_constructor x ctx with
  | None -> ()
  | Some _ -> error ~loc (ShadowConstructor x)

(** Desugar a singature of a computation type *)
let signature ~loc ctx lst =
  let rec fold ops sgs = function
    | [] -> Desugared.{ sig_ops=ops; sig_sgs=sgs }
    | x :: lst ->
       begin
         match lookup_ident x ctx with
         | None -> error ~loc (UnknownOperation x)
         | Some Variable -> error ~loc (OperationOrSignalExpected x)
         | Some Operation -> fold (Name.Set.add x ops) sgs lst
         | Some Signal -> fold ops (Name.Set.add x sgs) lst
       end
  in
  fold Name.Set.empty Name.Set.empty lst

let operations ops =
  List.fold_left (fun ops op -> Name.Set.add op ops) Name.Set.empty ops

let primitive = function
  | Sugared.Empty -> Desugared.Empty
  | Sugared.Int -> Desugared.Int
  | Sugared.Bool -> Desugared.Bool
  | Sugared.String -> Desugared.String
  | Sugared.Any -> Desugared.Any

(** Desugar a type, allowing named types to be from the given list. *)
let rec ty ctx {Location.it=t'; loc} =
  let t' =
    match t' with

    | Sugared.Primitive p -> Desugared.Primitive (primitive p)

    | Sugared.NamedTy t ->
       begin match lookup_ty t ctx with
       | Some TydefAlias -> Desugared.Alias t
       | Some TydefDatatype -> Desugared.Datatype t
       | Some TydefAbstract -> Desugared.Abstract t
       | None -> error ~loc (UnknownType t)
       end

    | Sugared.Product lst ->
       let lst = List.map (fun t -> ty ctx t) lst in
       Desugared.Product lst

    | Sugared.Arrow (t1, t2) ->
       let t1 = ty ctx t1
       and t2 = ty ctx t2 in
       Desugared.Arrow (t1, t2)

    | Sugared.RunnerTy (ops, t, sgn2) ->
       let ops = operations ops
       and t = ty ctx t
       and sgn2 = signature ~loc ctx sgn2 in
       Desugared.RunnerTy (ops, t, sgn2)

    | Sugared.ShellTy lst ->
       let rec fold ops = function
         | [] -> ops
         | op :: lst ->
            if is_operation op ctx then
              let ops = Name.Set.add op ops in
              fold ops lst
            else
              error ~loc (OperationExpected op)
       in
       let ops = fold Name.Set.empty lst in
       Desugared.ShellTy ops

    | Sugared.CompTy (t, sgn) ->
       let t = ty ctx t
       and sgn = signature ~loc ctx sgn in
       Desugared.CompTy (t, sgn)
  in
  Location.locate ~loc t'

let ty_opt ctx = function
  | None -> None
  | Some t -> Some (ty ctx t)

(** Desugar a pattern *)
let rec pattern ctx {Location.it=p'; loc} =
  let locate = Location.locate ~loc in
  match p' with

  | Sugared.PattAnonymous ->
     ctx, locate (Desugared.PattAnonymous)

  | Sugared.PattVar x ->
     let ctx = extend_ident x Variable ctx in
     ctx, locate (Desugared.PattVar x)

  | Sugared.PattNumeral n ->
     ctx, locate (Desugared.PattNumeral n)

  | Sugared.PattBoolean b ->
     ctx, locate (Desugared.PattBoolean b)

  | Sugared.PattQuoted s ->
     ctx, locate (Desugared.PattQuoted s)

  | Sugared.PattConstructor (cnstr, None) ->
     begin
       match lookup_constructor cnstr ctx with
       | None -> error ~loc (UnknownConstructor cnstr)
       | Some ConstrApplied -> error ~loc (ConstructorMustApply cnstr)
       | Some ConstrNonApplied -> ctx, locate (Desugared.PattConstructor (cnstr, None))
     end

  | Sugared.PattConstructor (cnstr, Some p) ->
     begin
       match lookup_constructor cnstr ctx with
       | None -> error ~loc (UnknownConstructor cnstr)
       | Some ConstrNonApplied -> error ~loc (ConstructorCannotApply cnstr)
       | Some ConstrApplied ->
          let ctx, p = pattern ctx p in
          ctx, locate (Desugared.PattConstructor (cnstr, Some p))
     end

  | Sugared.PattTuple ps ->
     let ctx, ps = pattern_tuple ctx ps in
     ctx, locate (Desugared.PattTuple ps)

and pattern_tuple ctx ps =
  let rec fold ctx qs = function

    | [] ->
       let qs = List.rev qs in
       ctx, qs

    | p :: ps ->
       let ctx, q = pattern ctx p in
       fold ctx (q :: qs) ps
  in
  fold ctx [] ps

(** Desugar an expression *)
let rec expr (ctx : context) ({Location.it=e'; Location.loc=loc} as e) =
  let locate x = Location.locate ~loc x in
  match e' with
    | Sugared.Var x ->
       begin
         match lookup_ident x ctx with
         | None -> error ~loc (UnknownIdentifier x)
         | Some Variable -> ([], locate (Desugared.Var x))
         | Some Operation -> error ~loc BareOperation
         | Some Signal -> error ~loc BareSignal
       end

    | Sugared.Ascribe (e, t) ->
       let w, e = expr ctx e in
       let t = ty ctx t in
       (w, locate (Desugared.AscribeExpr (e, t)))

    | Sugared.Numeral n ->
       ([], locate (Desugared.Numeral n))

    | Sugared.False ->
       ([], locate (Desugared.Boolean false))

    | Sugared.True ->
       ([], locate (Desugared.Boolean true))

    | Sugared.Quoted s ->
       ([], locate (Desugared.Quoted s))

    | Sugared.Constructor cnstr ->
       begin
         match lookup_constructor cnstr ctx with
         | None -> error ~loc (UnknownConstructor cnstr)
         | Some ConstrApplied -> error ~loc (ConstructorMustApply cnstr)
         | Some ConstrNonApplied -> ([], locate (Desugared.Constructor (cnstr, None)))
       end

    | Sugared.Apply ({Location.it=Sugared.Constructor cnstr;_}, e) ->
       begin
         match lookup_constructor cnstr ctx with
         | None -> error ~loc (UnknownConstructor cnstr)
         | Some ConstrNonApplied -> error ~loc (ConstructorCannotApply cnstr)
         | Some ConstrApplied ->
            let ws, e = expr ctx e in
            (ws, locate (Desugared.Constructor (cnstr, Some e)))
       end

    | Sugared.Tuple lst ->
       let ws, lst = expr_tuple ctx lst in
       (ws, locate (Desugared.Tuple lst))

    | Sugared.Lambda (pxs, c) ->
       ([], abstract ~loc ctx pxs c)

    | Sugared.Runner (t, lst) ->
       let t = ty ctx t in
       let lst = runner_clauses ~loc ctx lst in
       ([], locate (Desugared.Runner (t, lst)))

    | Sugared.RunnerTimes (e1, e2) ->
       let ws1, e1 = expr ctx e1
       and ws2, e2 = expr ctx e2 in
       (ws1 @ ws2, locate (Desugared.RunnerTimes (e1, e2)))

    | Sugared.RunnerRename (e, rnm) ->
       let ws, e = expr ctx e in
       let rnm =
         List.map
           (fun (x, y) ->
             let x =
               match lookup_ident x ctx with
               | None -> error ~loc (UnknownIdentifier x)
               | Some (Variable | Signal)-> error ~loc (OperationExpected x)
               | Some Operation -> x
             in
             let y =
               match lookup_ident y ctx with
               | None -> error ~loc (UnknownIdentifier x)
               | Some (Variable | Signal)-> error ~loc (OperationExpected x)
               | Some Operation -> y
             in
             (x, y))
           rnm
       in
       (ws, locate (Desugared.RunnerRename (e, rnm)))

    | (Sugared.Match _ | Sugared.If _ | Sugared.Apply _ | Sugared.Let _ |
       Sugared.LetRec _ | Sugared.Sequence _ | Sugared.Run _ | Sugared.Try _ | Sugared.Equal _) ->
       let c = comp ctx e in
       let x = Name.anonymous () in
       ([(x, c)], locate (Desugared.Var x))

and expr_tuple ctx es =
  let rec fold = function
    | [] -> [], []
    | t :: ts ->
       let w, e = expr ctx t
       and ws, es = fold ts in
       (w @ ws, e :: es)
  in
  fold es

(** Abstract 0 or more times to get a computation *)
and abstract0 ~loc ctx pxs c =
  let locate x = Location.locate ~loc x in
  let rec fold ctx = function
    | [] -> comp ctx c
    | px :: pxs ->
       let ctx, px = binder ctx px in
       let c = fold ctx pxs in
       locate (Desugared.Val (locate (Desugared.Lambda (px, c))))
  in
  fold ctx pxs

and abstract ~loc ctx pxs c =
  match pxs with
  | [] -> assert false

  | px :: pxs ->
     let ctx, px = binder ctx px in
     let c = abstract0 ~loc ctx pxs c in
     Location.locate ~loc (Desugared.Lambda (px, c))


and runner_clauses ~loc ctx lst = List.map (runner_clause ~loc ctx) lst

and runner_clause ~loc ctx (op, px, pw, c) =
  match lookup_ident op ctx with

  | None -> error ~loc (UnknownOperation op)

  | Some (Signal | Variable) -> error ~loc (OperationExpected op)

  | Some Operation ->
     let ctx, px = binder ctx px in
     let ctx, pw = binder ctx pw in
     let c = comp ctx c in
     (op, px, pw, c)

(** Desugar a computation *)
and comp ctx ({Location.it=c'; Location.loc=loc} as c) : Desugared.comp =
  let locate x = Location.locate ~loc x in
  let let_binds ws c =
    let rec fold = function
    | [] -> c
    | (x, cx) :: ws ->
       let let_cs = fold ws in
       locate (Desugared.Let (locate (Desugared.PattVar x), cx, let_cs))
    in
    fold ws
  in
  match c' with
    (* keep this case in front so that constructors are handled ocrrectly *)
    | (Sugared.Var _ | Sugared.Numeral _ | Sugared.False | Sugared.True | Sugared.Quoted _ |
       Sugared.Constructor _ | Sugared.Lambda _ | Sugared.Tuple _ | Sugared.Runner _ |
       Sugared.RunnerTimes _ | Sugared.RunnerRename _ |
       Sugared.Apply ({Location.it=Sugared.Constructor _;_}, _)) ->
       let ws, e = expr ctx c in
       let return_e = locate (Desugared.Val e) in
       let_binds ws return_e

    | Sugared.Ascribe (c, t) ->
       let c = comp ctx c in
       let t = ty ctx t in
       locate (Desugared.AscribeComp (c, t))

    | Sugared.Match (e, lst) ->
       let w, e = expr ctx e in
       let lst = match_clauses ctx lst in
       let_binds w (locate (Desugared.Match (e, lst)))

    | Sugared.If (e, c1, c2) ->
       (* Desguar into a match statement *)
       let w, e = expr ctx e in
       let e' =
         let loc x = Location.locate ~loc:e.Location.loc x in
         loc (Desugared.AscribeExpr (e, loc (Desugared.(Primitive Bool))))
       in
       let b = Location.locate ~loc (Desugared.(Primitive Bool)) in
       let cl1 =
         let c1 = comp ctx c1 in
         let loc x = Location.locate ~loc:c1.Location.loc x in
         ((loc (Desugared.PattBoolean true), Some b), c1)
       in
       let cl2 =
         let c2 = comp ctx c2 in
         let loc x = Location.locate ~loc:c1.Location.loc x in
         ((loc (Desugared.PattBoolean false), Some b), c2)
       in
       let_binds w (locate (Desugared.Match (e', [cl1; cl2])))

    | Sugared.Equal (e1, e2) ->
       let ws1, e1 = expr ctx e1 in
       let ws2, e2 = expr ctx e2 in
       let app = locate (Desugared.Equal (e1, e2)) in
       let_binds (ws1 @ ws2) app

    | Sugared.Apply ({Location.it=Sugared.Var op; _}, e) when is_operation op ctx ->
       let ws, e = expr ctx e in
       let c = locate (Desugared.Operation (op, e)) in
       let_binds ws c

    | Sugared.Apply ({Location.it=Sugared.Var sgl; _}, e) when is_signal sgl ctx ->
       let ws, e = expr ctx e in
       let c = locate (Desugared.Signal (sgl, e)) in
       let_binds ws c

    | Sugared.Apply (e1, e2) ->
       let ws1, e1 = expr ctx e1 in
       let ws2, e2 = expr ctx e2 in
       let app = locate (Desugared.Apply (e1, e2)) in
       let_binds (ws1 @ ws2) app

    | Sugared.(Let (BindVal (p, topt, c1), c2)) ->
       let c1 =
         match topt with
         | None -> comp ctx c1
         | Some t ->
            let t = ty ctx t in
            let c1 = comp ctx c1 in
            Location.locate ~loc:c1.Location.loc (Desugared.AscribeComp (c1, t))
       in
       let ctx, p = pattern ctx p in
       let c2 = comp ctx c2 in
       locate (Desugared.Let (p, c1, c2))

    | Sugared.(Let (BindFun (f, pxs, topt, c1), c2)) ->
       let c1 =
         match topt with
         | None -> c1
         | Some t -> Location.locate ~loc:c1.Location.loc (Sugared.Ascribe (c1, t))
       in
       let e = abstract ~loc ctx pxs c1 in
       let c1 = Location.locate ~loc:c1.Location.loc (Desugared.Val e) in
       let ctx = extend_ident f Variable ctx in
       let c2 = comp ctx c2 in
       let p = locate (Desugared.PattVar f) in
       locate (Desugared.Let (p, c1, c2))

    | Sugared.LetRec (lst, c) ->
       let ctx, lst = rec_clauses ~loc ctx lst in
       let c = comp ctx c in
       locate (Desugared.LetRec (lst, c))

    | Sugared.Sequence (c1, c2) ->
       let c1 = comp ctx c1 in
       let c2 = comp ctx c2 in
       let p = locate Desugared.PattAnonymous in
       locate (Desugared.Let (p, c1, c2))

    | Sugared.Run (e, w, c, fin) ->
       let ws1, e = expr ctx e in
       let ws2, w = expr ctx w in
       let c = comp ctx c in
       let fin = finally_clauses ~loc ctx fin in
       let_binds (ws1 @ ws2) (locate (Desugared.Run (e, w, c, fin)))

    | Sugared.Try (c, tr) ->
       let c = comp ctx c in
       let tr = try_clauses ~loc ctx tr in
       locate (Desugared.Try (c, tr))

and match_clauses ctx lst =
  List.map (match_clause ctx) lst

and match_clause ctx (p, c) =
  let ctx, p = binder ctx p in
  let c = comp ctx c in
  (p, c)

and rec_clauses ~loc ctx lst =
  let ctx =
    List.fold_left
      (fun ctx (f, _, _, _, _) -> extend_ident f Variable ctx)
      ctx
      lst
  in
  let rec fold clauses = function
    | [] -> ctx, List.rev clauses

    | (f, (p, u), pus, s, c) :: lst ->
       let u = ty ctx u in
       let t =
         List.fold_right
         (fun (_, u) t ->
           let u = ty ctx u in
           Location.locate ~loc:t.Location.loc (Desugared.Arrow (u, t)))
         pus
         (ty ctx s)
       in
       let ctx, p = pattern ctx p in
       let c = abstract0 ~loc ctx (List.map (fun (p, _) -> (p, None)) pus) c
       in
       fold ((f, t, p, u, c) :: clauses) lst
  in
  fold [] lst

and finally_clauses ~loc ctx lst =
  let rec fold fin_val fin_signals = function

    | [] ->
       begin match fin_val with
       | None -> error ~loc MissingFinallyVal
       | Some fin_val ->
          let fin_signals = List.rev fin_signals in
          { Desugared.fin_val; Desugared.fin_signals }
       end

    | Sugared.FinVal (px, pw, c) :: lst ->
       begin match fin_val with
       | Some _ -> error ~loc DoubleFinallyVal
       | None ->
          let ctx, px = binder ctx px in
          let ctx, pw = binder ctx pw in
          let c = comp ctx c in
          let fin_val = Some (px, pw, c) in
          fold fin_val fin_signals lst
       end

    | Sugared.FinSignal (sgl, px, pw, c) :: lst ->
       begin match List.exists (fun (sgl', _, _, _) -> Name.equal sgl sgl') fin_signals with
       | true -> error ~loc (DoubleFinallySignal sgl)
       | false ->
          if not (is_signal sgl ctx) then error ~loc (SignalExpected sgl) ;
          let ctx, px = binder ctx px in
          let ctx, pw = binder ctx pw in
          let c = comp ctx c in
          let fin_signals = (sgl, px, pw, c) :: fin_signals in
          fold fin_val fin_signals lst
       end
  in
  fold None [] lst

and try_clauses ~loc ctx lst =
  let rec fold try_val try_signals = function

    | [] ->
       begin match try_val with
       | None -> error ~loc MissingFinallyVal
       | Some try_val ->
          let try_signals = List.rev try_signals in
          Desugared.{try_val; try_signals}
       end

    | Sugared.TryVal (px, c) :: lst ->
       begin match try_val with
       | Some _ -> error ~loc DoubleFinallyVal
       | None ->
          let ctx, px = binder ctx px in
          let c = comp ctx c in
          let try_val = Some (px, c) in
          fold try_val try_signals lst
       end

    | Sugared.TrySignal (sgl, px, c) :: lst ->
       begin match List.exists (fun (sgl', _, _) -> Name.equal sgl sgl') try_signals with
       | true -> error ~loc (DoubleFinallySignal sgl)
       | false ->
          if not (is_signal sgl ctx) then error ~loc (SignalExpected sgl) ;
          let ctx, px = binder ctx px in
          let c = comp ctx c in
          let try_signals = (sgl, px, c) :: try_signals in
          fold try_val try_signals lst
       end
  in
  fold None [] lst

(** Desugar a single binder. *)
and binder ctx (p, topt) =
  let ctx, p = pattern ctx p in
  let topt = ty_opt ctx topt in
  ctx, (p, topt)

(** Desugar the clauses of a datatype definition. *)
let datatype ~loc ctx lst =
  let rec fold ctx clauses = function

    | [] ->
       let clauses = List.rev clauses in
       ctx, clauses

    | (cnstr, None) :: lst ->
       check_constructor_shadow ~loc cnstr ctx ;
       let ctx = extend_constructor cnstr ConstrNonApplied ctx in
       fold ctx ((cnstr, None) :: clauses) lst

    | (cnstr, Some t) :: lst ->
       check_constructor_shadow ~loc cnstr ctx ;
       let t = ty ctx t in
       let ctx = extend_constructor cnstr ConstrApplied ctx in
       fold ctx ((cnstr, Some t) :: clauses) lst
  in
  fold ctx [] lst

(** Desugar a type definition *)
let datatypes ~loc ctx ty_defs =
  let ctx =
    List.fold_left
    (fun ctx (t, _) ->
      check_type_shadow ~loc t ctx ;
      extend_type t TydefDatatype ctx)
    ctx ty_defs
  in
  List.fold_left
    (fun (ctx, ty_defs) (t, cnstrs) ->
      let ctx, cnstrs = datatype ~loc ctx cnstrs in
      ctx, (t, cnstrs) :: ty_defs)
    (ctx, [])
    ty_defs

(** Desugar a toplevel. *)
let rec toplevel ctx {Location.it=c; Location.loc=loc} =

(** Desugar a non-located toplevel. *)
let toplevel' ctx = function

    | Sugared.TopLoad fn ->
       let ctx, cmds = load ctx fn in
       ctx, Desugared.TopLoad cmds

    | Sugared.(TopLet (BindVal (p, topt, c))) ->
       let c =
         match topt with
         | None -> comp ctx c
         | Some t ->
            let t = ty ctx t in
            let c1 = comp ctx c in
            Location.locate ~loc:c1.Location.loc (Desugared.AscribeComp (c1, t))
       and ctx, p = pattern ctx p in
       ctx, Desugared.TopLet (p, c)

    | Sugared.(TopLet (BindFun (f, a, topt, c))) ->
       let c =
         match topt with
         | None -> c
         | Some t -> Location.locate ~loc:c.Location.loc (Sugared.Ascribe (c, t))
       in
       let e = abstract ~loc ctx a c in
       let c = Location.locate ~loc:c.Location.loc (Desugared.Val e) in
       let ctx = extend_ident f Variable ctx in
       let p = Location.locate ~loc (Desugared.PattVar f) in
       ctx, Desugared.TopLet (p, c)

    | Sugared.TopLetRec lst ->
       let ctx, lst = rec_clauses ~loc ctx lst in
       ctx, Desugared.TopLetRec lst

    | Sugared.TopShell c ->
       let c = comp ctx c in
       ctx, Desugared.TopShell c

    | Sugared.TopComp c ->
       let c = comp ctx c in
       ctx, Desugared.TopComp c

    | Sugared.DefineAlias (t, abbrev) ->
       check_type_shadow ~loc t ctx ;
       let abbrev = ty ctx abbrev in
       let ctx = extend_type t TydefAlias ctx in
       ctx, Desugared.DefineAlias (t, abbrev)

    | Sugared.DefineAbstract t ->
       check_type_shadow ~loc t ctx ;
       let ctx = extend_type t TydefAbstract ctx in
       ctx, Desugared.DefineAbstract t

    | Sugared.DefineDatatype lst ->
       let ctx, lst = datatypes ~loc ctx lst in
       ctx, Desugared.DefineDatatype lst

    | Sugared.DeclareOperation (op, t1, t2) ->
       check_ident_shadow ~loc op ctx ;
       let t1 = ty ctx t1
       and t2 = ty ctx t2
       and ctx = extend_ident op Operation ctx in
       ctx, Desugared.DeclareOperation (op, t1, t2)

    | Sugared.DeclareSignal (sgl, t) ->
       check_ident_shadow ~loc sgl ctx ;
       let t = ty ctx t in
       let ctx = extend_ident sgl Signal ctx in
       ctx, Desugared.DeclareSignal (sgl, t)

    | Sugared.External (x, t, s) ->
       let t = ty ctx t in
       let ctx = extend_ident x Variable ctx in
       ctx, Desugared.External (x, t, s)

  in
  let ctx, c = toplevel' ctx c in
  ctx, Location.locate ~loc c

(** Load a file and desugar it. *)
and load ctx fn =
  let cmds = Lexer.read_file Parser.file fn in
  let ctx, cmds = List.fold_left
                    (fun (ctx,cmds) cmd ->
                      let ctx, cmd = toplevel ctx cmd in
                      (ctx, cmd::cmds))
                    (ctx,[]) cmds
  in
  let cmds = List.rev cmds in
  ctx, cmds
