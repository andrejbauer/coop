(** Conversion from parsed syntax to abstract syntax.

    The desugaring phase loads required files (but does not run them),
    it converts variable names to de Bruijn indices, and it converts
    the complex abstractions of [Input] to the simple ones of [Syntax].
*)

(** Desugaring errors *)
type desugar_error =
  | UnknownIdentifier of Name.ident

(** The exception signalling a desugaring error*)
exception Error of desugar_error Location.located

(** [error ~loc err] raises the given desugaring error. *)
let error ~loc err = Pervasives.raise (Error (Location.locate ~loc err))

(** Print desugaring error. *)
let print_error err ppf =
  match err with
  | UnknownIdentifier x -> Format.fprintf ppf "unknown identifier %t" (Name.print_ident x)

(** A desugaring context is a list of known identifiers, which is used to compute de
   Bruijn indices. *)
type context = Name.ident list

(** Initial empty context *)
let initial = []

(** Add a new identifier to the context. *)
let extend x ctx = x :: ctx

(** Is [x] a known identifier? *)
let known x ctx = List.exists (Name.equal x) ctx

(** Desugar a type, which at this stage is the same as an expressions. *)
let rec ty = function

  | Input.Int -> Dsyntax.Int

  | Input.Product lst ->
     let lst = List.map ty lst in
     Dsyntax.Product lst

  | Input.Arrow (t1, t2) ->
     let t1 = ty t1
     and t2 = ty t2 in
     Dsyntax.Arrow (t1, t2)

let ty_opt = function
  | None -> None
  | Some t -> Some (ty t)

(** Desugar a pattern *)
let rec pattern ctx {Location.data=p'; loc} =
  let locate = Location.locate ~loc in
  match p' with

  | Input.PattAnonymous ->
     ctx, locate (Dsyntax.PattAnonymous)

  | Input.PattVar x ->
     let ctx = extend x ctx in
     ctx, locate (Dsyntax.PattVar x)

  | Input.PattNumeral n ->
     ctx, locate (Dsyntax.PattNumeral n)

  | Input.PattTuple lst ->
     let rec fold ctx qs = function

       | [] ->
          let qs = List.rev qs in
          ctx, locate (Dsyntax.PattTuple qs)

       | p :: ps ->
          let ctx, q = pattern ctx p in
          fold ctx (q :: qs) ps
     in
     fold ctx [] lst

(** Desugar an expression *)
let rec expr ctx ({Location.data=e'; Location.loc=loc} as e) =
  let locate x = Location.locate ~loc x in
  match e' with

    | Input.Var x ->
       begin match known x ctx with
       | false -> error ~loc (UnknownIdentifier x)
       | true -> ([], locate (Dsyntax.Var x))
       end

    | Input.Numeral n ->
       ([], locate (Dsyntax.Numeral n))

    | Input.Tuple lst ->
       let rec fold = function
         | [] -> [], []
         | t :: ts ->
            let w, e = expr ctx t
            and ws, es = fold ts in
            (w @ ws, e :: es)
       in
       let ws, lst = fold lst in
       (ws, locate (Dsyntax.Tuple lst))

    | Input.Lambda (a, c) ->
       let ctx, lst = lambda_abstraction ctx a in
       let c = comp ctx c in
       let rec fold = function
         | [] -> assert false
         | [(x,topt)] -> locate (Dsyntax.Lambda (x, topt, c))
         | (x,topt) :: lst ->
            let e = fold lst in
            let c = locate (Dsyntax.Return e) in
            locate (Dsyntax.Lambda (x, topt, c))
       in
       ([], fold lst)

    | Input.Ascribe (e, t) ->
       let w, e = expr ctx e in
       let t = ty t in
       (w, locate (Dsyntax.AscribeExpr (e, t)))

    | (Input.Match _ | Input.Apply _ | Input.Let _) ->
       let c = comp ctx e in
       let x = Name.anonymous () in
       ([(x, c)], locate (Dsyntax.Var x))

(** Desugar a computation *)
and comp ctx ({Location.data=c'; Location.loc=loc} as c) : Dsyntax.comp =
  let locate x = Location.locate ~loc x in
  let let_binds ws c =
    let rec fold = function
    | [] -> c
    | (x,c) :: ws ->
       let let_cs = fold ws in
       locate (Dsyntax.Let (locate (Dsyntax.PattVar x), c, let_cs))
    in
    fold ws
  in
  match c' with
    | (Input.Var _ | Input.Numeral _ | Input.Lambda _ | Input.Tuple _) ->
       let ws, e = expr ctx c in
       let return_e = locate (Dsyntax.Return e) in
       let_binds ws return_e

    | Input.Match (e, lst) ->
       let w, e = expr ctx e in
       let lst = match_clauses ctx lst in
       let_binds w (locate (Dsyntax.Match (e, lst)))

    | Input.Apply (e1, e2) ->
       let ws1, e1 = expr ctx e1 in
       let ws2, e2 = expr ctx e2 in
       let app = locate (Dsyntax.Apply (e1, e2)) in
       let_binds (ws1 @ ws2) app

    | Input.Let (p, c1, c2) ->
       let c1 = comp ctx c1 in
       let ctx', p = pattern ctx p in
       let c2 = comp ctx' c2 in
       locate (Dsyntax.Let (p, c1, c2))

    | Input.Ascribe (c, t) ->
       let c = comp ctx c in
       let t = ty t in
       locate (Dsyntax.AscribeComp (c, t))

and match_clauses ctx lst =
  List.map (match_clause ctx) lst

and match_clause ctx (patt, c) =
  let ctx, patt = pattern ctx patt in
  let c = comp ctx c in
  (patt, c)

(** Desugar a lambda abstraction. *)
and lambda_abstraction ctx a : context * (Name.ident * Dsyntax.ty option) list =
  let rec fold ctx = function
    | [] -> ctx, []
    | (xs, topt) :: lst ->
       let ctx, xts = lambda_abstraction1 ctx xs topt in
       let ctx, yts = fold ctx lst in
       ctx, xts @ yts
  in
  fold ctx a

(** Auxiliary function used to desugar lambda abstractions. *)
and lambda_abstraction1 ctx xs topt : context * (Name.ident * Dsyntax.ty option) list =
  let topt = ty_opt topt in
  let rec fold ctx lst = function
    | [] -> ctx, List.rev lst
    | x :: xs ->
       let ctx = extend x ctx
       and lst = (x, topt) :: lst in
       fold ctx lst xs
  in
  fold ctx [] xs


(** Desugar a toplevel. *)
let rec toplevel ctx {Location.data=c; Location.loc=loc} =

(** Desugar a non-located toplevel. *)
let toplevel' ctx = function

    | Input.TopLoad fn ->
       let ctx, cmds = load ctx fn in
       ctx, Dsyntax.TopLoad cmds

    | Input.TopLet(p, c) ->
       let c = comp ctx c
       and ctx, p = pattern ctx p in
       ctx, Dsyntax.TopLet (p, c)

    | Input.TopComp c ->
       let c = comp ctx c in
       ctx, Dsyntax.TopComp c

    | Input.DeclOperation (op, t1, t2) ->
       let t1 = ty t1
       and t2 = ty t2
       and ctx = extend op ctx in
       ctx, Dsyntax.DeclOperation (op, t1, t2)

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
