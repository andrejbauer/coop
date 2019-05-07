(** Conversion from parsed syntax to abstract syntax.

    The desugaring phase loads required files (but does not run them),
    it converts variable names to de Bruijn indices, and it converts
    the complex abstractions of [Input] to the simple ones of [Syntax].
*)

(** Desugaring errors *)
type desugar_error =
  | UnknownIdentifier of Name.t
  | UnknownOperation of Name.t
  | OperationExpected of Name.t
  | BareOperation

(** The exception signalling a desugaring error*)
exception Error of desugar_error Location.located

(** [error ~loc err] raises the given desugaring error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

(** Print desugaring error. *)
let print_error err ppf =
  match err with
  | UnknownIdentifier x -> Format.fprintf ppf "unknown identifier %t" (Name.print x)
  | UnknownOperation op -> Format.fprintf ppf "unknown operation %t" (Name.print op)
  | OperationExpected x -> Format.fprintf ppf "%t is not an operation" (Name.print x)
  | BareOperation -> Format.fprintf ppf "this operation should be applied to an argument"

(** A desugaring context resolves names to their kinds. *)
type ident_kind = Variable | Operation

type context = ident_kind Name.Map.t

(** Initial empty context *)
let initial = Name.Map.empty

(** Add a new identifier of the given kind to the context. *)
let extend = Name.Map.add

let is_operation op ctx =
  match Name.Map.find op ctx with
  | Some Operation -> true
  | Some Variable | None -> false

let signature ~loc ctx lst =
  let rec fold sgn = function
    | [] -> sgn
    | op :: ops ->
       begin
         match Name.Map.find op ctx with
         | None -> error ~loc (UnknownOperation op)
         | Some Variable -> error ~loc (OperationExpected op)
         | Some Operation -> fold (Name.Set.add op sgn) ops
       end
  in
  fold Name.Set.empty lst

(** Desugar a type, which at this stage is the same as an expressions. *)
let rec ty ctx {Location.data=t'; loc} =
  let t' =
    match t' with

    | Sugared.Int -> Desugared.Int

    | Sugared.Product lst ->
       let lst = List.map (ty ctx) lst in
       Desugared.Product lst

    | Sugared.Arrow (t1, t2) ->
       let t1 = ty ctx t1
       and t2 = ty ctx t2 in
       Desugared.Arrow (t1, t2)

    | Sugared.ComodelTy (sgn1, t, sgn2) ->
       let sgn1 = signature ~loc ctx sgn1
       and t = ty ctx t
       and sgn2 = signature ~loc ctx sgn2 in
       Desugared.ComodelTy (sgn1, t, sgn2)

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
let rec pattern ctx {Location.data=p'; loc} =
  let locate = Location.locate ~loc in
  match p' with

  | Sugared.PattAnonymous ->
     ctx, locate (Desugared.PattAnonymous)

  | Sugared.PattVar x ->
     let ctx = extend x Variable ctx in
     ctx, locate (Desugared.PattVar x)

  | Sugared.PattNumeral n ->
     ctx, locate (Desugared.PattNumeral n)

  | Sugared.PattTuple lst ->
     let rec fold ctx qs = function

       | [] ->
          let qs = List.rev qs in
          ctx, locate (Desugared.PattTuple qs)

       | p :: ps ->
          let ctx, q = pattern ctx p in
          fold ctx (q :: qs) ps
     in
     fold ctx [] lst

(** Desugar an expression *)
let rec expr (ctx : context) ({Location.data=e'; Location.loc=loc} as e) =
  let locate x = Location.locate ~loc x in
  match e' with
    | Sugared.Var x ->
       begin
         match Name.Map.find x ctx with
         | None -> error ~loc (UnknownIdentifier x)
         | Some Variable -> ([], locate (Desugared.Var x))
         | Some Operation -> error ~loc BareOperation
       end

    | Sugared.Ascribe (e, t) ->
       let w, e = expr ctx e in
       let t = ty ctx t in
       (w, locate (Desugared.AscribeExpr (e, t)))

    | Sugared.Numeral n ->
       ([], locate (Desugared.Numeral n))

    | Sugared.Tuple lst ->
       let rec fold = function
         | [] -> [], []
         | t :: ts ->
            let w, e = expr ctx t
            and ws, es = fold ts in
            (w @ ws, e :: es)
       in
       let ws, lst = fold lst in
       (ws, locate (Desugared.Tuple lst))

    | Sugared.Lambda (a, c) ->
       ([], abstract ~loc ctx a c)

    | Sugared.Comodel (e, lst) ->
       let ws, e = expr ctx e in
       let lst = comodel_clauses ~loc ctx lst in
       (ws, locate (Desugared.Comodel (e, lst)))

    | (Sugared.Match _ | Sugared.Apply _ | Sugared.Let _ | Sugared.Sequence _ |
       Sugared.LetFun _ | Sugared.Using _) ->
       let c = comp ctx e in
       let x = Name.anonymous () in
       ([(x, c)], locate (Desugared.Var x))

and abstract ~loc ctx a c =
  let locate x = Location.locate ~loc x in
  let ctx, lst = binders ctx a in
  let c = comp ctx c in
  let rec fold = function
    | [] -> assert false
    | [(x, topt)] -> locate (Desugared.Lambda ((x, topt), c))
    | (x, topt) :: lst ->
       let e = fold lst in
       let c = locate (Desugared.Val e) in
       locate (Desugared.Lambda ((x, topt), c))
  in
  fold lst

and comodel_clauses ~loc ctx lst = List.map (comodel_clause ~loc ctx) lst

and comodel_clause ~loc ctx (op, px, pw, c) =
  if not (is_operation op ctx) then
    error ~loc (UnknownOperation op)
  else
    let ctx, px = pattern ctx px in
    let ctx, pw = pattern ctx pw in
    let c = comp ctx c in
    (op, px, pw, c)

(** Desugar a computation *)
and comp ctx ({Location.data=c'; Location.loc=loc} as c) : Desugared.comp =
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
    | (Sugared.Var _ | Sugared.Numeral _ | Sugared.Lambda _ | Sugared.Tuple _ | Sugared.Comodel _) ->
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

    | Sugared.Apply ({Location.data=Sugared.Var op; loc}, e) when is_operation op ctx ->
       let ws, e = expr ctx e in
       let c = locate (Desugared.Operation (op, e)) in
       let_binds ws c

    | Sugared.Apply (e1, e2) ->
       let ws1, e1 = expr ctx e1 in
       let ws2, e2 = expr ctx e2 in
       let app = locate (Desugared.Apply (e1, e2)) in
       let_binds (ws1 @ ws2) app

    | Sugared.Let (p, c1, c2) ->
       let c1 = comp ctx c1 in
       let ctx, p = pattern ctx p in
       let c2 = comp ctx c2 in
       locate (Desugared.Let (p, c1, c2))

    | Sugared.Sequence (c1, c2) ->
       let c1 = comp ctx c1 in
       let c2 = comp ctx c2 in
       let p = locate Desugared.PattAnonymous in
       locate (Desugared.Let (p, c1, c2))

    | Sugared.LetFun (f, a, c1, c2) ->
       let e = abstract ~loc ctx a c1 in
       let c1 = Location.locate ~loc:c1.Location.loc (Desugared.Val e) in
       let ctx = extend f Variable ctx in
       let c2 = comp ctx c2 in
       let p = locate (Desugared.PattVar f) in
       locate (Desugared.Let (p, c1, c2))

    | Sugared.Using (cmdl, c, fs) ->
       let ws, cmdl = expr ctx cmdl in
       let c = comp ctx c in
       let fs = finally ctx fs in
       let_binds ws (locate (Desugared.Using (cmdl, c, fs)))

and match_clauses ctx lst =
  List.map (match_clause ctx) lst

and match_clause ctx (patt, c) =
  let ctx, patt = pattern ctx patt in
  let c = comp ctx c in
  (patt, c)

and finally ctx (px, pw, c) =
  let ctx, px = pattern ctx px in
  let ctx, pw = pattern ctx pw in
  let c = comp ctx c in
  (px, pw, c)

(** Desugar a lambda abstraction. *)
and binders ctx a =
  let rec fold ctx pts = function
    | [] -> ctx, List.rev pts
    | (p, topt) :: lst ->
       let ctx, p = pattern ctx p in
       let topt = ty_opt ctx topt in
       fold ctx ((p, topt) :: pts) lst
  in
  fold ctx [] a

(** Desugar a toplevel. *)
let rec toplevel ctx {Location.data=c; Location.loc=loc} =

(** Desugar a non-located toplevel. *)
let toplevel' ctx = function

    | Sugared.TopLoad fn ->
       let ctx, cmds = load ctx fn in
       ctx, Desugared.TopLoad cmds

    | Sugared.TopLet (p, c) ->
       let c = comp ctx c
       and ctx, p = pattern ctx p in
       ctx, Desugared.TopLet (p, c)

    | Sugared.TopLetFun (f, a, c) ->
       let e = abstract ~loc ctx a c in
       let c = Location.locate ~loc:c.Location.loc (Desugared.Val e) in
       let ctx = extend f Variable ctx in
       let p = Location.locate ~loc (Desugared.PattVar f) in
       ctx, Desugared.TopLet (p, c)

    | Sugared.TopComp c ->
       let c = comp ctx c in
       ctx, Desugared.TopComp c

    | Sugared.DeclOperation (op, t1, t2) ->
       let t1 = ty ctx t1
       and t2 = ty ctx t2
       and ctx = extend op Operation ctx in
       ctx, Desugared.DeclOperation (op, t1, t2)

    | Sugared.External (x, t, s) ->
       let t = ty ctx t in
       let ctx = extend x Variable ctx in
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
