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

(** The next de Bruijn index to be used for hoisting of computations from expressions. *)
let debruijn binds = List.length binds

(** Find the de Bruijn index of [x] in the context [ctx]. *)
let index x ctx =
  let rec search k = function
    | [] -> None
    | y :: ys ->
     if x = y then Some k else search (k+1) ys
  in
  search 0 ctx

(** Desugar a type, which at this stage is the same as an expressions. *)
let rec ty = function
  | Input.Int -> Dsyntax.Int
  | Input.Arrow (t1, t2) ->
     let t1 = ty t1
     and t2 = ty t2 in
     Dsyntax.Arrow (t1, t2)

let ty_opt = function
  | None -> None
  | Some t -> Some (ty t)

(** Desugar an expression *)
let rec expr ctx binds ({Location.data=e'; Location.loc=loc} as e) =
  match e' with

    | Input.Var x ->
       begin match index x ctx with
       | None -> error ~loc (UnknownIdentifier x)
       | Some k -> (binds, Location.locate ~loc (Dsyntax.Var k))
       end

    | Input.Numeral n ->
       (binds, Location.locate ~loc (Dsyntax.Numeral n))

    | Input.Lambda (a, c) ->
       let ctx, lst = lambda_abstraction ctx a in
       let c = comp ctx c in
       let rec fold = function
         | [] -> assert false
         | [(x,topt)] -> Location.locate ~loc (Dsyntax.Lambda (x, topt, c))
         | (x,topt) :: lst ->
            let e = fold lst in
            let c = Location.locate ~loc (Dsyntax.Return e) in
            Location.locate ~loc (Dsyntax.Lambda (x, topt, c))
       in
       (binds, fold lst)

    | (Input.Apply _ | Input.Let _) ->
       let c = comp ctx e in
       let k = debruijn binds in
       (c :: binds, Location.locate ~loc (Dsyntax.Var k))

(** Desugar a computation *)
and comp ctx ({Location.data=c'; Location.loc=loc} as c) : Dsyntax.comp =
  let let_binds binds c =
    let rec fold = function
    | [] -> c
    | b :: binds ->
       let let_cs = fold binds in
       let x = Name.anonymous () in
       Location.locate ~loc (Dsyntax.Sequence (x, c, let_cs))
    in
    fold (List.rev binds)
  in
  match c' with
    | (Input.Var _ | Input.Numeral _ | Input.Lambda _) ->
       let binds, e = expr ctx [] c in
       let return_e = Location.locate ~loc (Dsyntax.Return e) in
       let_binds binds return_e

    | Input.Apply (e1, e2) ->
       let binds, e1 = expr ctx [] e1 in
       let binds, e2 = expr ctx [] e2 in
       let app = Location.locate ~loc (Dsyntax.Apply (e1, e2)) in
       let_binds binds app

    | Input.Let (x, c1, c2) ->
       let c1 = comp ctx c1 in
       let c2 = comp (extend x ctx) c2 in
       Location.locate ~loc (Dsyntax.Sequence (x, c1, c2))

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

    | Input.TopLet(x, c) ->
       let c = comp ctx c
       and ctx = extend x ctx in
       ctx, Dsyntax.TopLet (x, c)

    | Input.TopComp c ->
       let c = comp ctx c in
       ctx, Dsyntax.TopComp c

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
