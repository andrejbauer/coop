(** Desugaring errors *)
type desugar_error =
  | UnknownIdentifier of Name.t
  | UnknownResource of Name.t
  | UnknownConstructor of Name.t
  | UnknownType of Name.t
  | UnexpectedResource of Name.t
  | UnexpectedSignal of Name.t
  | UnexpectedException of Name.t
  | UnexpectedComputationTy
  | ResourceExpected of Name.t
  | ExceptionExpected of Name.t
  | SignalExpected of Name.t
  | ShadowResource of Name.t
  | ShadowException of Name.t
  | ShadowSignal of Name.t
  | ShadowConstructor of Name.t
  | ConstructorCannotApply of Name.t
  | ConstructorMustApply of Name.t
  | ShadowType of Name.t
  | BareResource
  | BareException
  | BareSignal
  | MissingFinallyVal
  | DoubleVal
  | DoubleException of Name.t
  | DoubleSignal of Name.t

(** The exception signalling a desugaring error*)
exception Error of desugar_error Location.located

(** [error ~loc err] raises the given desugaring error. *)
let error ~loc err = Stdlib.raise (Error (Location.locate ~loc err))

(** Print desugaring error. *)
let print_error err ppf =
  match err with

  | UnknownIdentifier x ->
     Format.fprintf ppf "unknown identifier %t" (Name.print x)

  | UnknownResource op ->
     Format.fprintf ppf "unknown resource %t" (Name.print op)

  | UnknownConstructor cnstr ->
     Format.fprintf ppf "unknown constructor %t" (Name.print cnstr)

  | UnknownType ty ->
     Format.fprintf ppf "unknown type %t" (Name.print ty)

  | UnexpectedResource op ->
     Format.fprintf ppf "resourrce %t cannot appear here" (Print.exception_name op)

  | UnexpectedException sgn ->
     Format.fprintf ppf "exception %t cannot appear here" (Print.exception_name sgn)

  | UnexpectedSignal sgn ->
     Format.fprintf ppf "signal %t cannot appear here" (Print.signal_name sgn)

  | UnexpectedComputationTy ->
     Format.fprintf ppf "expected an expression type here"

  | ResourceExpected x ->
     Format.fprintf ppf "%t is not a resource" (Name.print x)

  | SignalExpected x ->
     Format.fprintf ppf "%t is not a signal" (Name.print x)

  | ExceptionExpected x ->
     Format.fprintf ppf "%t is not an exception" (Name.print x)

  | ShadowResource op ->
     Format.fprintf ppf "%t is already declared to be a resource" (Name.print op)

  | ShadowException exc ->
     Format.fprintf ppf "%t is already declared to be an exception" (Name.print exc)

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

  | BareResource ->
     Format.fprintf ppf "this resource should be applied to an argument"

  | BareException ->
     Format.fprintf ppf "if you want to raise an exception, prefix it with !"

  | BareSignal ->
     Format.fprintf ppf "if you want to send a signal, prefix it with %s" (Print.char_bangbang ())

  | MissingFinallyVal ->
     Format.fprintf ppf "missing finally value clause"

  | DoubleVal ->
     Format.fprintf ppf "multiple value clauses"

  | DoubleException sgl ->
     Format.fprintf ppf "exception %t has multiple clauses" (Name.print sgl)

  | DoubleSignal sgl ->
     Format.fprintf ppf "signal %t has multiple clauses" (Name.print sgl)


(** A desugaring context resolves names to their kinds. *)
type ident_kind =
  | Variable
  | Resource
  | Exception
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

let is_resource op ctx =
  match lookup_ident op ctx with
  | Some Resource -> true
  | Some (Variable | Exception | Signal) | None -> false

let check_resource ~loc op ctx =
  match lookup_ident op ctx with
  | Some Resource -> ()
  | Some (Variable | Exception | Signal) | None ->
     error ~loc (ResourceExpected op)

let check_signal ~loc sgl ctx =
  match lookup_ident sgl ctx with
  | Some Signal -> ()
  | Some (Variable | Exception | Resource) | None ->
     error ~loc (SignalExpected sgl)

let check_exception ~loc exc ctx =
  match lookup_ident exc ctx with
  | Some Exception -> ()
  | Some (Variable | Signal | Resource) | None ->
     error ~loc (ExceptionExpected exc)

(** Check whether [x] shadows a resource or a signal *)
let check_ident_shadow ~loc x ctx =
  match lookup_ident x ctx with
  | None | Some Variable -> ()
  | Some Resource -> error ~loc (ShadowResource x)
  | Some Exception -> error ~loc (ShadowException x)
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

let primitive = function
  | Sugared.Empty -> Desugared.Empty
  | Sugared.Int -> Desugared.Int
  | Sugared.Bool -> Desugared.Bool
  | Sugared.String -> Desugared.String

(** Manipulation of signatures *)

let resource_exception_signal_signature ~loc ctx sg =
  let rec fold ops exc sgn = function

    | [] ->
       Desugared.(Resources ops, Exceptions exc, Signals sgn)

    | Sugared.Resource o :: sg ->
       check_resource ~loc o ctx ;
       let ops = Name.Set.add o ops in
       fold ops exc sgn sg

    | Sugared.Exception e :: sg ->
       check_exception ~loc e ctx ;
       let exc = Name.Set.add e exc in
       fold ops exc sgn sg

    | Sugared.Signal s :: sg ->
       check_signal ~loc s ctx ;
       let sgn = Name.Set.add s sgn in
       fold ops exc sgn sg
  in
  fold Name.Set.empty Name.Set.empty Name.Set.empty sg

let resource_exception_signature ~loc ctx sg =
  let rec fold ops exc = function

    | [] -> Desugared.(Resources ops, Exceptions exc)

    | Sugared.Resource o :: sg ->
       check_resource ~loc o ctx ;
       let ops = Name.Set.add o ops in
       fold ops exc sg

    | Sugared.Exception e :: sg ->
       check_exception ~loc e ctx ;
       let exc = Name.Set.add e exc in
       fold ops exc sg

    | Sugared.Signal s :: _ ->
       check_signal ~loc s ctx ;
       error ~loc (UnexpectedSignal s)
  in
  fold Name.Set.empty Name.Set.empty sg

let resource_signal_signature ~loc ctx sg =
  let rec fold ops sgn = function

    | [] -> Desugared.(Resources ops, Signals sgn)

    | Sugared.Resource o :: sg ->
       check_resource ~loc o ctx ;
       let ops = Name.Set.add o ops in
       fold ops sgn sg

    | Sugared.Exception e :: _ ->
       check_exception ~loc e ctx ;
       error ~loc (UnexpectedException e)

    | Sugared.Signal s :: _ ->
       check_signal ~loc s ctx ;
       let sgn = Name.Set.add s sgn in
       fold ops sgn sg
  in
  fold Name.Set.empty Name.Set.empty sg

let resource_signature ~loc ctx sg =
  let rec fold ops = function

    | [] -> Desugared.Resources ops

    | Sugared.Resource o :: sg ->
       check_resource ~loc o ctx ;
       let ops = Name.Set.add o ops in
       fold ops sg

    | Sugared.Exception e :: _ ->
       check_exception ~loc e ctx ;
       error ~loc (UnexpectedException e)

    | Sugared.Signal s :: _ ->
       check_signal ~loc s ctx ;
       error ~loc (UnexpectedSignal s)
  in
  fold Name.Set.empty sg

let exception_signature ~loc ctx sg =
  let rec fold exc = function

    | [] -> Desugared.Exceptions exc

    | Sugared.Resource o :: sg ->
       check_resource ~loc o ctx ;
       error ~loc (UnexpectedResource o)

    | Sugared.Exception e :: sg ->
       check_exception ~loc e ctx ;
       let exc = Name.Set.add e exc in
       fold exc sg

    | Sugared.Signal s :: _ ->
       check_signal ~loc s ctx ;
       error ~loc (UnexpectedSignal s)
  in
  fold Name.Set.empty sg


(* Auxiliary type for figuring out arrow types. *)
type user_or_kernel_ty =
  | UserTy of Desugared.user_ty
  | KernelTy of Desugared.kernel_ty

(** Desugar an expression type, allowing named types to be from the given list. *)
let rec expr_ty ctx {Location.it=t'; loc} =
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
       let lst = List.map (fun t -> expr_ty ctx t) lst in
       Desugared.Product lst

    | Sugared.Arrow (t1, t2) ->
       let t1 = expr_ty ctx t1 in
       begin match user_or_kernel_ty ctx t2 with
       | UserTy t2 -> Desugared.ArrowUser (t1, t2)
       | KernelTy t2 -> Desugared.ArrowKernel (t1, t2)
       end

    | Sugared.RunnerTy (sg1, sg2, wt) ->
       let ops1 = resource_signature ~loc ctx sg1
       and wt = expr_ty ctx wt
       and ops2, sgn = resource_signal_signature ~loc ctx sg2 in
       Desugared.RunnerTy (ops1, ops2, sgn, wt)

    | Sugared.ContainerTy sg ->
       let ops = resource_signature ~loc ctx sg in
       Desugared.ContainerTy ops

    | Sugared.ComputationTy _ ->
       error ~loc UnexpectedComputationTy
  in
  Location.locate ~loc t'

and user_or_kernel_ty ctx ({Location.it=t'; loc} as t) =
  match t' with

  | Sugared.(Primitive _ | NamedTy _ | Product _ | Arrow _ | RunnerTy _ | ContainerTy _) ->
     let t = expr_ty ctx t in
     UserTy (Location.locate ~loc (Desugared.{user_ty = t;
                                              user_res = Resources Name.Set.empty;
                                              user_exc = Exceptions Name.Set.empty}))

  | Sugared.ComputationTy (t, sg, None) ->
     let user_ty = expr_ty ctx t
     and user_res, user_exc = resource_exception_signature ~loc ctx sg in
     UserTy (Location.locate ~loc (Desugared.{user_ty; user_res; user_exc}))

  | Sugared.ComputationTy (t, sg, Some wt) ->
     let kernel_ty = expr_ty ctx t
     and kernel_res, kernel_exc, kernel_sgn = resource_exception_signal_signature ~loc ctx sg
     and kernel_world = expr_ty ctx wt in
     KernelTy (Location.locate ~loc (Desugared.{kernel_ty; kernel_res; kernel_exc; kernel_sgn; kernel_world}))


let expr_ty_opt ctx = function
  | None -> None
  | Some t -> Some (expr_ty ctx t)

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
         | Some Resource -> error ~loc BareResource
         | Some Exception -> error ~loc BareException
         | Some Signal -> error ~loc BareSignal
       end

    | Sugared.Ascribe (e, t) ->
       let w, e = expr ctx e in
       let t = expr_ty ctx t in
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

    | Sugared.FunUser (pxs, c) ->
       ([], user_abstract ~loc ctx pxs c)

    | Sugared.FunKernel (pxs, wt, c) ->
       let wt = expr_ty ctx wt in
       ([], kernel_abstract ~loc ctx pxs wt c)

    | Sugared.Runner (lst, t) ->
       let t = expr_ty ctx t in
       let lst = runner_clauses ~loc ctx lst in
       ([], locate (Desugared.Runner (lst, t)))

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
               | Some (Variable | Exception | Signal)-> error ~loc (ResourceExpected x)
               | Some Resource -> x
             in
             let y =
               match lookup_ident y ctx with
               | None -> error ~loc (UnknownIdentifier x)
               | Some (Variable | Exception | Signal)-> error ~loc (ResourceExpected x)
               | Some Resource -> y
             in
             (x, y))
           rnm
       in
       (ws, locate (Desugared.RunnerRename (e, rnm)))

    | Sugared.(Match _ | If _ | Apply _ | Let _ | LetRec _ |
               Sequence _ | Using _ | Try _ | Equal _ | Getenv | Setenv _ | Raise _ | Kill _ |
               ExecUser _ | ExecKernel _) ->
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

(** Abstract 0 or more times to get a user computation *)
and user_abstract0 ~loc ctx pxs c =
  let locate x = Location.locate ~loc x in
  let rec fold ctx = function
    | [] -> comp ctx c
    | px :: pxs ->
       let ctx, px = binder ctx px in
       let c = fold ctx pxs in
       locate (Desugared.Return (locate (Desugared.FunUser (px, c))))
  in
  fold ctx pxs

(** Abstract 1 or more times to get a user function *)
and user_abstract ~loc ctx pxs c =
  match pxs with
  | [] -> assert false

  | px :: pxs ->
     let ctx, px = binder ctx px in
     let c = user_abstract0 ~loc ctx pxs c in
     Location.locate ~loc (Desugared.FunUser (px, c))

(** Abstract 0 or more times to get a kernel computation *)
and kernel_abstract0 ~loc ctx pxs wt c =
  let locate x = Location.locate ~loc x in
  let rec fold ctx = function
    | [] -> comp ctx c
    | px :: pxs ->
       let ctx, px = binder ctx px in
       let c = fold ctx pxs in
       locate (Desugared.Return (locate (Desugared.FunKernel (px, wt, c))))
  in
  fold ctx pxs

(** Abstract 1 or more times to get a kernel function *)
and kernel_abstract ~loc ctx pxs wt c =
  match pxs with
  | [] -> assert false

  | px :: pxs ->
     let ctx, px = binder ctx px in
     let c = kernel_abstract0 ~loc ctx pxs wt c in
     Location.locate ~loc (Desugared.FunKernel (px, wt, c))

and runner_clauses ~loc ctx lst = List.map (runner_clause ~loc ctx) lst

and runner_clause ~loc ctx (op, px, c) =
  match lookup_ident op ctx with

  | None -> error ~loc (UnknownResource op)

  | Some (Exception | Signal | Variable) -> error ~loc (ResourceExpected op)

  | Some Resource ->
     let ctx, px = binder ctx px in
     let c = comp ctx c in
     (op, px, c)

(** Desugar a user computation *)
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
    | Sugared.(Var _ | Numeral _ | False | True | Quoted _ | Constructor _ | FunUser _ | FunKernel _ |
               Tuple _ | Runner _ | Sugared.RunnerTimes _ | Sugared.RunnerRename _ |
               Apply ({Location.it=Constructor _;_}, _)) ->
       let ws, e = expr ctx c in
       let return_e = locate (Desugared.Return e) in
       let_binds ws return_e

    | Sugared.Ascribe (c, t) ->
       let c = comp ctx c in
       begin match user_or_kernel_ty ctx t with
       | UserTy t -> locate (Desugared.AscribeUser (c, t))
       | KernelTy t -> locate (Desugared.AscribeKernel (c, t))
       end

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

    | Sugared.Apply ({Location.it=Sugared.Var op; _}, e) when is_resource op ctx ->
       let ws, e = expr ctx e in
       let c = locate (Desugared.Resource (op, e)) in
       let_binds ws c

    | Sugared.Apply (e1, e2) ->
       let ws1, e1 = expr ctx e1 in
       let ws2, e2 = expr ctx e2 in
       let app = locate (Desugared.Apply (e1, e2)) in
       let_binds (ws1 @ ws2) app

    | Sugared.(Let (BindVal (p, c1), c2)) ->
       let c1 = comp ctx c1 in
       let ctx, p = pattern ctx p in
       let c2 = comp ctx c2 in
       locate (Desugared.Let (p, c1, c2))

    | Sugared.(Let (BindFunUser (f, pxs, c1), c2)) ->
       let e = user_abstract ~loc ctx pxs c1 in
       let c1 = Location.locate ~loc:c1.Location.loc (Desugared.Return e) in
       let ctx = extend_ident f Variable ctx in
       let c2 = comp ctx c2 in
       let p = locate (Desugared.PattVar f) in
       locate (Desugared.Let (p, c1, c2))

    | Sugared.(Let (BindFunKernel (f, pxs, t, c1), c2)) ->
       let t = expr_ty ctx t in
       let e = kernel_abstract ~loc ctx pxs t c1 in
       let c1 = Location.locate ~loc:c1.Location.loc (Desugared.Return e) in
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

    | Sugared.Getenv ->
       locate Desugared.Getenv

    | Sugared.Setenv e ->
       let ws, e = expr ctx e in
       let_binds ws (locate (Desugared.Setenv e))

    | Sugared.Raise (exc, e) ->
       check_exception ~loc exc ctx ;
       let ws, e = expr ctx e in
       let_binds ws (locate (Desugared.Raise (exc, e)))

    | Sugared.Kill (sgn, e) ->
       check_signal ~loc sgn ctx ;
       let ws, e = expr ctx e in
       let_binds ws (locate (Desugared.Kill (sgn, e)))

    | Sugared.Using (e, w, c, fin) ->
       let ws1, e = expr ctx e in
       let ws2, w = expr ctx w in
       let c = comp ctx c in
       let fin = finally_clauses ~loc ctx fin in
       let_binds (ws1 @ ws2) (locate (Desugared.Using (e, w, c, fin)))

    | Sugared.Try (c, hnd) ->
       let c = comp ctx c in
       let hnd = try_clauses ~loc ctx hnd in
       locate (Desugared.Try (c, hnd))

    | Sugared.ExecUser (c, hnd) ->
       let c = comp ctx c in
       let hnd = try_clauses ~loc ctx hnd in
       locate (Desugared.ExecUser (c, hnd))

    | Sugared.ExecKernel (c, w, fin) ->
       let ws, w = expr ctx w in
       let c = comp ctx c in
       let fin = finally_clauses ~loc ctx fin in
       let_binds ws (locate (Desugared.ExecKernel (c, w, fin)))

and match_clauses ctx lst =
  List.map (match_clause ctx) lst

and match_clause ctx (p, c) =
  let ctx, p = binder ctx p in
  let c = comp ctx c in
  (p, c)


and rec_clause ctx (f, (p, u), t, c) =
  let u = expr_ty ctx u in
  let c, p =
    let ctx, p = pattern ctx p in
    let c = comp ctx c in
    c, p
  in
  begin match user_or_kernel_ty ctx t with
  | UserTy t -> Desugared.RecUser (f, p, u, t, c)
  | KernelTy t -> Desugared.RecKernel (f, p, u, t, c)
  end

and rec_clauses ~loc ctx lst =
  let ctx =
    List.fold_left
      (fun ctx (f, _, _, _) -> extend_ident f Variable ctx)
      ctx
      lst
  in
  let lst = List.map (rec_clause ctx) lst in
  ctx, lst

and finally_clauses ~loc ctx lst =
  let rec fold fin_return fin_raise fin_kill = function

    | [] ->
       begin match fin_return with
       | None -> error ~loc MissingFinallyVal
       | Some fin_return ->
          let fin_kill = List.rev fin_kill in
          Desugared.{fin_return; fin_raise; fin_kill}
       end

    | Sugared.FinVal (px, pw, c) :: lst ->
       begin match fin_return with
       | Some _ -> error ~loc DoubleVal
       | None ->
          let ctx, px = binder ctx px in
          let ctx, pw = binder ctx pw in
          let c = comp ctx c in
          let fin_return = Some (px, pw, c) in
          fold fin_return fin_raise fin_kill lst
       end

    | Sugared.FinRaise (exc, px, pw, c) :: lst ->
       begin match List.exists (fun (exc', _, _, _) -> Name.equal exc exc') fin_raise with
       | true -> error ~loc (DoubleException exc)
       | false ->
          check_exception ~loc exc ctx ;
          let ctx, px = binder ctx px in
          let ctx, pw = binder ctx pw in
          let c = comp ctx c in
          let fin_raise = (exc, px, pw, c) :: fin_raise in
          fold fin_return fin_raise fin_kill lst
       end

    | Sugared.FinKill (sgl, px, c) :: lst ->
       begin match List.exists (fun (sgl', _, _) -> Name.equal sgl sgl') fin_kill with
       | true -> error ~loc (DoubleSignal sgl)
       | false ->
          check_signal ~loc sgl ctx ;
          let ctx, px = binder ctx px in
          let c = comp ctx c in
          let fin_kill = (sgl, px, c) :: fin_kill in
          fold fin_return fin_raise fin_kill lst
       end
  in
  fold None [] [] lst

and try_clauses ~loc ctx lst =
  let rec fold try_return try_raise = function

    | [] ->
       let try_raise = List.rev try_raise in
       Desugared.{try_return; try_raise}

    | Sugared.TryVal (px, c) :: lst ->
       begin match try_return with
       | Some _ -> error ~loc DoubleVal
       | None ->
          let ctx, px = binder ctx px in
          let c = comp ctx c in
          let try_return = Some (px, c) in
          fold try_return try_raise lst
       end

    | Sugared.TryRaise (exc, px, c) :: lst ->
       begin match List.exists (fun (exc', _, _) -> Name.equal exc exc') try_raise with
       | true -> error ~loc (DoubleException exc)
       | false ->
          check_exception ~loc exc ctx ;
          let ctx, px = binder ctx px in
          let c = comp ctx c in
          let try_raise = (exc, px, c) :: try_raise in
          fold try_return try_raise lst
       end
  in
  fold None [] lst

(** Desugar a single binder. *)
and binder ctx (p, topt) =
  let ctx, p = pattern ctx p in
  let topt = expr_ty_opt ctx topt in
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
       let t = expr_ty ctx t in
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

    | Sugared.(TopLet (BindVal (p, c))) ->
       let c = comp ctx c
       and ctx, p = pattern ctx p in
       ctx, Desugared.TopLet (p, c)

    | Sugared.(TopLet (BindFunUser (f, a, c))) ->
       let e = user_abstract ~loc ctx a c in
       let c = Location.locate ~loc:c.Location.loc (Desugared.Return e) in
       let ctx = extend_ident f Variable ctx in
       let p = Location.locate ~loc (Desugared.PattVar f) in
       ctx, Desugared.TopLet (p, c)

    | Sugared.(TopLet (BindFunKernel (f, a, t, c))) ->
       let t = expr_ty ctx t in
       let e = kernel_abstract ~loc ctx a t c in
       let c = Location.locate ~loc:c.Location.loc (Desugared.Return e) in
       let ctx = extend_ident f Variable ctx in
       let p = Location.locate ~loc (Desugared.PattVar f) in
       ctx, Desugared.TopLet (p, c)

    | Sugared.TopLetRec lst ->
       let ctx, lst = rec_clauses ~loc ctx lst in
       ctx, Desugared.TopLetRec lst

    | Sugared.TopContainer cs ->
       let cs = List.map (comp ctx) cs in
       ctx, Desugared.TopContainer cs

    | Sugared.TopUser c ->
       let c = comp ctx c in
       ctx, Desugared.TopUser c

    | Sugared.DefineAlias (t, abbrev) ->
       check_type_shadow ~loc t ctx ;
       let abbrev = expr_ty ctx abbrev in
       let ctx = extend_type t TydefAlias ctx in
       ctx, Desugared.DefineAlias (t, abbrev)

    | Sugared.DefineAbstract t ->
       check_type_shadow ~loc t ctx ;
       let ctx = extend_type t TydefAbstract ctx in
       ctx, Desugared.DefineAbstract t

    | Sugared.DefineDatatype lst ->
       let ctx, lst = datatypes ~loc ctx lst in
       ctx, Desugared.DefineDatatype lst

    | Sugared.DeclareResource (op, t1, t2, sg) ->
       check_ident_shadow ~loc op ctx ;
       let t1 = expr_ty ctx t1
       and t2 = expr_ty ctx t2
       and exc = exception_signature ~loc ctx sg in
       let ctx = extend_ident op Resource ctx in
       ctx, Desugared.DeclareResource (op, t1, t2, exc)

    | Sugared.DeclareException (exc, t) ->
       check_ident_shadow ~loc exc ctx ;
       let t = expr_ty ctx t in
       let ctx = extend_ident exc Exception ctx in
       ctx, Desugared.DeclareException (exc, t)

    | Sugared.DeclareSignal (sgl, t) ->
       check_ident_shadow ~loc sgl ctx ;
       let t = expr_ty ctx t in
       let ctx = extend_ident sgl Signal ctx in
       ctx, Desugared.DeclareSignal (sgl, t)

    | Sugared.External (x, t, s) ->
       let t = expr_ty ctx t in
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
