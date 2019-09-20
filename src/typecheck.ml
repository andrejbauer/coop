(** Coop type checking. *)

(** Typing context *)
type context =
  { ctx_operations : (Syntax.expr_ty * Syntax.expr_ty * Syntax.exceptions) Name.Map.t
  ; ctx_signals : Syntax.expr_ty Name.Map.t
  ; ctx_exceptions : Syntax.expr_ty Name.Map.t
  ; ctx_idents : (Name.t * Syntax.expr_ty) list
  ; ctx_aliases : Syntax.expr_ty Name.Map.t
  ; ctx_datatypes : (Name.t * (Name.t * Syntax.expr_ty option) list) list
  ; ctx_container : Name.Set.t
  }

(** Initial typing context *)
let initial =
  { ctx_operations = Name.Map.empty
  ; ctx_exceptions = Name.Map.empty
  ; ctx_signals = Name.Map.empty
  ; ctx_idents = []
  ; ctx_aliases = Name.Map.empty
  ; ctx_datatypes = []
  ; ctx_container = Name.Set.empty
  }

(** Type errors *)
type error =
  | InvalidName of Name.t
  | InvalidOperation of Name.t
  | InvalidSignal of Name.t
  | InvalidException of Name.t
  | InvalidType of Name.t
  | PattTypeMismatch of Syntax.expr_ty
  | ExprTypeMismatch of Syntax.expr_ty * Syntax.expr_ty
  | WorldTypeMismatch of Syntax.expr_ty * Syntax.expr_ty
  | CoopTypeMismatch of Syntax.expr_ty * Syntax.expr_ty
  | ExprTypeExpected
  | WrongTupleLength of int * int
  | IllegalConstructor of Name.t
  | UnknownConstructor of Name.t
  | UserTypeMismatch of Syntax.user_ty * Syntax.user_ty
  | KernelTypeMismatch of Syntax.kernel_ty * Syntax.kernel_ty
  | TypeExpectedButFunction of Syntax.expr_ty
  | TypeExpectedButTuple of Syntax.expr_ty
  | TypeExpectedButUnit of Syntax.expr_ty
  | FunctionExpected of Syntax.expr_ty
  | RunnerExpected of Syntax.expr_ty
  | ContainerExpected of Syntax.expr_ty
  | RunnerDoubleOperations of Name.Set.t
  | CannotInferArgument
  | CannotInferMatch
  | CannotInferWorld
  | DuplicateOperation of Name.t
  | DuplicateSignal of Name.t
  | DuplicateException of Name.t
  | UnhandledOperations of Name.Set.t
  | UnhandledExceptions of Name.Set.t
  | UnhandledSignals of Name.Set.t

exception Error of error Location.located

let print_error err ppf =
  match err with

  | InvalidName x -> Format.fprintf ppf "invalid name %t, please report" (Name.print x)

  | InvalidOperation op ->
     Format.fprintf ppf "invalid operation %t, please report"
       (Name.print op)

  | InvalidSignal sgl ->
     Format.fprintf ppf "invalid signal %t, please report"
       (Name.print sgl)

  | InvalidException exc ->
     Format.fprintf ppf "invalid exception %t, please report"
       (Name.print exc)

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

  | WorldTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "the world of this runner should have type@ %t but has type@ %t"
       (Syntax.print_expr_ty ty_expected)
       (Syntax.print_expr_ty ty_actual)

  | CoopTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this co-operation should return values of type@ %t but it returns values of type@ %t"
       (Syntax.print_expr_ty ty_expected)
       (Syntax.print_expr_ty ty_actual)

  | UserTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this computation should have type@ %t but has type@ %t"
       (Syntax.print_user_ty ty_expected)
       (Syntax.print_user_ty ty_actual)

  | KernelTypeMismatch (ty_expected, ty_actual) ->
     Format.fprintf ppf "this computation should have type@ %t but has type@ %t"
       (Syntax.print_kernel_ty ty_expected)
       (Syntax.print_kernel_ty ty_actual)

  | ExprTypeExpected ->
     Format.fprintf ppf "this type should be pure"

  | WrongTupleLength (k_expected, k_actual) ->
     Format.fprintf ppf "this tuple should have %d component but has %d" k_expected k_actual

  | IllegalConstructor cnstr ->
     Format.fprintf ppf "illegal application of constructor %t, please report"
       (Name.print cnstr)

  | UnknownConstructor cnstr ->
     Format.fprintf ppf "unknown constructor %t" (Name.print cnstr)

  | TypeExpectedButFunction ty ->
     Format.fprintf ppf "this expression is a function but should have type@ %t"
                        (Syntax.print_expr_ty ty)

  | TypeExpectedButTuple ty ->
     Format.fprintf ppf "this expression is a tuple but should have type@ %t"
       (Syntax.print_expr_ty ty)

  | TypeExpectedButUnit ty ->
     Format.fprintf ppf "this expression is the unit but should have type@ %t"
       (Syntax.print_expr_ty ty)

  | FunctionExpected ty ->
     Format.fprintf ppf "this expression should be a function but has type@ %t"
       (Syntax.print_expr_ty ty)

  | RunnerExpected ty ->
     Format.fprintf ppf "this expression should be a runner but has type@ %t"
       (Syntax.print_expr_ty ty)

  | ContainerExpected ty ->
     Format.fprintf ppf "this expression should be a container but has type@ %t"
       (Syntax.print_expr_ty ty)

  | RunnerDoubleOperations ops ->
     let ops = Name.Set.elements ops in
     Format.fprintf ppf "these runners both handle the following operations:@ %t"
       (Print.sequence (Name.print ~parentheses:true) "," ops)

  | CannotInferArgument ->
     Format.fprintf ppf "cannot infer the type of this argument"

  | CannotInferWorld ->
     Format.fprintf ppf "cannot infer the type of the world"

  | CannotInferMatch ->
     Format.fprintf ppf "cannot infer the type of this match statement"

  | DuplicateOperation op ->
     Format.fprintf ppf "operation %t is defined twice"
       (Name.print op)

  | DuplicateSignal sgl ->
     Format.fprintf ppf "signal %t is intercepted more than once"
       (Name.print sgl)

  | DuplicateException exc ->
     Format.fprintf ppf "exception %t is intercepted more than once"
       (Name.print exc)

  | UnhandledOperations ops ->
     Format.fprintf ppf "the following operations are potentially unhandled:@ %t"
       (Print.names ops)

  | UnhandledExceptions exc ->
     Format.fprintf ppf "the following exceptions are potentially unhandled:@ %t"
       (Print.names exc)

  | UnhandledSignals sgn ->
     Format.fprintf ppf "the following signals are potentially unhandled:@ %t"
       (Print.names sgn)

(** [error ~loc err] raises the given type-checking error. *)
let error ~loc err = Stdlib.raise (Error (Location.locate ~loc err))

(** [warning ~loc err] warns about the given type-checking error. *)
let warning ~loc err =
  Print.warning "@[<hov>Type warning at %t:@ %t@]@."
                (Location.print loc)
                (print_error err)

(** Extend the context with the type of deBruijn index 0 *)
let extend_ident x ty ctx =
  { ctx with ctx_idents = (x, ty) :: ctx.ctx_idents }

let rec extend_idents xts ctx =
  match xts with
  | [] -> ctx
  | (x, t) :: xts -> extend_idents xts (extend_ident x t ctx)

let extend_alias x ty ctx =
  { ctx with ctx_aliases = Name.Map.add x ty ctx.ctx_aliases }

let extend_datatype x cnstrs ctx =
  { ctx with ctx_datatypes = (x, cnstrs) :: ctx.ctx_datatypes }

let set_container ops ctx =
  { ctx with ctx_container = ops }

let declare_operation op ty1 ty2 excs ctx =
  { ctx with ctx_operations = Name.Map.add op (ty1, ty2, excs) ctx.ctx_operations }

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

(** Lookup the type of an operation. *)
let lookup_operation ~loc op {ctx_operations;_} =
  match Name.Map.find op ctx_operations with
  | None -> error ~loc (InvalidOperation op)
  | Some (ty1, ty2, excs) -> (ty1, ty2, excs)

(** Lookup the type of a signal *)
let lookup_signal ~loc sgl {ctx_signals;_} =
  match Name.Map.find sgl ctx_signals with
  | None -> error ~loc (InvalidSignal sgl)
  | Some ty -> ty

(** Lookup the type of an exception *)
let lookup_exception ~loc exc {ctx_exceptions;_} =
  match Name.Map.find exc ctx_exceptions with
  | None -> error ~loc (InvalidException exc)
  | Some ty -> ty

(** Lookup a type alias *)
let lookup_alias ~loc ty {ctx_aliases;_} =
  match Name.Map.find ty ctx_aliases with
  | None -> error ~loc (InvalidType ty)
  | Some abbrev -> abbrev

(** Lookup a datatype definition *)
let lookup_datatype ~loc ty {ctx_datatypes;_} =
  match List.assoc_opt ty ctx_datatypes with
  | None -> error ~loc (InvalidType ty)
  | Some def -> def

(** Lookup a datatype constructor *)
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

(** Lookup the type of a container *)
let lookup_container {ctx_container;_} = ctx_container

(**** Normalization of types ****)

type 'a normal = Normal of 'a

(** Unfold the definition of a type *)
let rec norm_ty ~loc ctx t =
  match t with

  | Syntax.Alias x ->
     let t = lookup_alias ~loc x ctx in
     norm_ty ~loc ctx t

  | Syntax.(Abstract _ | Datatype _ |  Primitive _ |
            Product _ | ArrowUser _ | ArrowKernel _ | RunnerTy _ | ContainerTy _) ->
     Normal t

let as_product (Normal ty) =
  match ty with

  | Syntax.Alias _ -> assert false

  | Syntax.Product ts -> Some ts

  | Syntax.(Abstract _ | Datatype _  | Primitive _ |
            ArrowUser _ | ArrowKernel _ | RunnerTy _ | ContainerTy _) ->
     None

let as_user_arrow (Normal ty) =
  match ty with

  | Syntax.Alias _ -> assert false

  | Syntax.ArrowUser (t, u) -> Some (t, u)

  | Syntax.(Product _ | Datatype _ | Primitive _ | ArrowKernel _ |
            Abstract _ | RunnerTy _ | ContainerTy _) ->
     None

let as_kernel_arrow (Normal ty) =
  match ty with

  | Syntax.Alias _ -> assert false

  | Syntax.ArrowKernel (t, u) -> Some (t, u)

  | Syntax.(Product _ | Datatype _ | Primitive _ | ArrowUser _ |
            Abstract _ | RunnerTy _ | ContainerTy _) ->
     None

let as_runner (Normal ty) =
  match ty with

  | Syntax.Alias _ -> assert false

  | Syntax.RunnerTy (ops1, ops2, sgs, w_ty) -> Some (ops1, ops2, sgs, w_ty)

  | Syntax.(Product _ | Datatype _ | Primitive _ |
            ArrowUser _ | ArrowKernel _ | Abstract _ | ContainerTy _) ->
     None

let as_container (Normal ty) =
  match ty with

  | Syntax.Alias _ -> assert false

  | Syntax.ContainerTy ops -> Some ops

  | Syntax.(Product _ | Datatype _ | Primitive _ |
            ArrowUser _ | ArrowKernel _ | Abstract _ | RunnerTy _) ->
     None

(**** Subtyping ****)

let rec expr_subty ~loc ctx t u =
  match t, u with

  | Syntax.Alias x, Syntax.Alias y when Name.equal x y -> true

  | Syntax.Alias _, _ ->
     let (Normal t) = norm_ty ~loc ctx t in
     expr_subty ~loc ctx t u

  | _, Syntax.Alias _ ->
     let (Normal u) = norm_ty ~loc ctx u in
     expr_subty ~loc ctx t u

  | Syntax.Datatype x, Syntax.Datatype y ->
     Name.equal x y

  | Syntax.(Primitive Empty), _ -> true

  | _, Syntax.(Primitive Empty) -> false

  | Syntax.(Primitive Any), _ -> false

  | _, Syntax.(Primitive Any) -> true

  | Syntax.Abstract t1, Syntax.Abstract t2 -> Name.equal t1 t2

  | Syntax.Primitive p1, Syntax.Primitive p2 -> p1 = p2

  | Syntax.Product ts, Syntax.Product us ->
     let rec fold ts us =
       match ts, us with
       | [], [] -> true
       | t :: ts, u :: us -> expr_subty ~loc ctx t u && fold ts us
       | [], _::_ | _::_, [] -> false
     in
     fold ts us

  | Syntax.ArrowUser (t1, t2), Syntax.ArrowUser (u1, u2) ->
     expr_subty ~loc ctx u1 t1 && user_subty ~loc ctx t2 u2

  | Syntax.(RunnerTy (Operations ops1, Operations ops1', Signals sgn1, w_ty1)),
    Syntax.(RunnerTy (Operations ops2, Operations ops2', Signals sgn2, w_ty2)) ->
     Name.Set.subset ops2 ops1 &&
     Name.Set.subset ops1' ops2' &&
     Name.Set.subset sgn1 sgn2 &&
    expr_eqtype ~loc ctx w_ty1 w_ty2

  | Syntax.(ContainerTy (Operations ops1)), Syntax.(ContainerTy (Operations ops2)) ->
     Name.Set.subset ops2 ops1

  | Syntax.(Datatype _ | Primitive _ | Product _ | ArrowUser _ |
            ArrowKernel _ | RunnerTy _ | Abstract _ | ContainerTy _), _ ->
     false

and user_subty ~loc ctx
  Syntax.{user_ty=t1; user_ops=Operations ops1; user_exc=Exceptions excs1}
  Syntax.{user_ty=t2; user_ops=Operations ops2; user_exc=Exceptions excs2}
 =
  Name.Set.subset ops1 ops2 && Name.Set.subset excs1 excs2 && expr_subty ~loc ctx t1 t2

and kernel_subty ~loc ctx
  Syntax.{kernel_ty=t1; kernel_ops=Operations ops1; kernel_exc=Exceptions excs1; kernel_sgn=Signals sgs1; kernel_world=w_ty1}
  Syntax.{kernel_ty=t2; kernel_ops=Operations ops2; kernel_exc=Exceptions excs2; kernel_sgn=Signals sgs2; kernel_world=w_ty2}
 =
  expr_subty ~loc ctx t1 t2 &&
  Name.Set.subset ops1 ops2 &&
  Name.Set.subset excs1 excs2 &&
  Name.Set.subset sgs1 sgs2 &&
  expr_eqtype ~loc ctx w_ty1 w_ty2

and expr_eqtype ~loc ctx t u =
  expr_subty ~loc ctx t u && expr_subty ~loc ctx u t

let join_operations (Syntax.Operations ops1) (Syntax.Operations ops2) =
  Syntax.Operations (Name.Set.union ops1 ops2)

let meet_operations (Syntax.Operations ops1) (Syntax.Operations ops2) =
  Syntax.Operations (Name.Set.inter ops1 ops2)

let join_exceptions (Syntax.Exceptions exc1) (Syntax.Exceptions exc2) =
  Syntax.Exceptions (Name.Set.union exc1 exc2)

let meet_exceptions (Syntax.Exceptions exc1) (Syntax.Exceptions exc2) =
  Syntax.Exceptions (Name.Set.inter exc1 exc2)

let join_signals (Syntax.Signals sgs1) (Syntax.Signals sgs2) =
  Syntax.Signals (Name.Set.union sgs1 sgs2)

let meet_signals (Syntax.Signals sgs1) (Syntax.Signals sgs2) =
  Syntax.Signals (Name.Set.inter sgs1 sgs2)

let rec join_expr_ty ~loc ctx t1 t2 =
  match t1, t2 with

  | Syntax.Alias x, Syntax.Alias y when Name.equal x y ->
     t1

  | Syntax.Alias _, _ ->
     let (Normal t1) = norm_ty ~loc ctx t1 in
     join_expr_ty ~loc ctx t1 t2

  | _, Syntax.Alias _ ->
     let (Normal t2) = norm_ty ~loc ctx t2 in
     join_expr_ty ~loc ctx t1 t2

  | Syntax.Datatype x, Syntax.Datatype y when Name.equal x y ->
     t1

  | Syntax.Abstract x, Syntax.Abstract y when Name.equal x y ->
     t1

  | Syntax.(Primitive Empty), t2 -> t2

  | t1, Syntax.(Primitive Empty) -> t1

  | Syntax.Primitive p1, Syntax.Primitive p2 when p1 = p2 -> t1

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

  | Syntax.ArrowUser (u1, t1), Syntax.ArrowUser (u2, t2) ->
     let u = meet_expr_ty ~loc ctx u1 u2
     and t = join_user_ty ~loc ctx t1 t2 in
     Syntax.ArrowUser (u, t)

  | Syntax.ArrowKernel (u1, t1), Syntax.ArrowKernel (u2, t2) ->
     let u = meet_expr_ty ~loc ctx u1 u2 in
     let t = join_kernel_ty ~loc ctx t1 t2 in
     Syntax.ArrowKernel (u, t)

  | Syntax.RunnerTy (ops1, ops1', sgn1, w_ty1),
    Syntax.RunnerTy (ops2, ops2', sgn2, w_ty2) ->
     let ops = meet_operations ops1 ops2
     and ops' = join_operations ops1' ops2'
     and sgn = join_signals sgn1 sgn2 in
     if expr_eqtype ~loc ctx w_ty1 w_ty2 then
       Syntax.RunnerTy (ops, ops', sgn, w_ty1)
     else
       error ~loc (WorldTypeMismatch (w_ty1, w_ty2))

  | Syntax.ContainerTy ops1, Syntax.ContainerTy ops2 ->
     let ops = meet_operations ops1 ops2 in
     Syntax.ContainerTy ops

  | Syntax.(Datatype _ | Primitive _ | Product _ |
            ArrowUser _ | ArrowKernel _ | RunnerTy _ | Abstract _ | ContainerTy _), _ ->
     error ~loc (ExprTypeMismatch (t2, t2))

and meet_expr_ty ~loc ctx t1 t2 =
  match t1, t2 with

  | Syntax.Alias x, Syntax.Alias y when Name.equal x y ->
     t1

  | Syntax.Alias _, _ ->
     let (Normal t1) = norm_ty ~loc ctx t1 in
     meet_expr_ty ~loc ctx t1 t2

  | _, Syntax.Alias _ ->
     let (Normal t2) = norm_ty ~loc ctx t2 in
     meet_expr_ty ~loc ctx t1 t2

  | Syntax.Datatype x, Syntax.Datatype y when Name.equal x y ->
     t1

  | Syntax.Abstract x, Syntax.Abstract y when Name.equal x y ->
     t1

  | Syntax.(Primitive Empty), _ -> Syntax.(Primitive Empty)

  | _, Syntax.(Primitive Empty) -> Syntax.(Primitive Empty)

  | Syntax.Primitive p1, Syntax.Primitive p2 when p1 = p2 -> t1

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

  | Syntax.ArrowUser (u1, t1), Syntax.ArrowUser (u2, t2) ->
     let u = join_expr_ty ~loc ctx u1 u2 in
     let t = meet_user_ty ~loc ctx t1 t2 in
     Syntax.ArrowUser (u, t)

  | Syntax.ArrowKernel (u1, t1), Syntax.ArrowKernel (u2, t2) ->
     let u = join_expr_ty ~loc ctx u1 u2 in
     let t = meet_kernel_ty ~loc ctx t1 t2 in
     Syntax.ArrowKernel (u, t)

  | Syntax.RunnerTy (ops1, ops1', sgn1, w_ty1),
    Syntax.RunnerTy (ops2, ops2', sgn2, w_ty2) ->
     let ops = join_operations ops1 ops2
     and ops' = meet_operations ops1' ops2'
     and sgn = meet_signals sgn1 sgn2 in
     if expr_eqtype ~loc ctx w_ty1 w_ty2 then
       Syntax.RunnerTy (ops, ops', sgn, w_ty1)
     else
       error ~loc (WorldTypeMismatch (w_ty1, w_ty2))

  | Syntax.ContainerTy ops1, Syntax.ContainerTy ops2 ->
     let ops = join_operations ops1 ops2 in
     Syntax.ContainerTy ops

  | Syntax.(Datatype _ | Primitive _ | Product _ | ArrowUser _ | ArrowKernel _ |
            RunnerTy _ | Abstract _ | ContainerTy _), _ ->
     error ~loc (ExprTypeMismatch (t2, t2))

and join_user_ty ~loc ctx
  Syntax.{user_ty=t1; user_ops=ops1; user_exc=exc1}
  Syntax.{user_ty=t2; user_ops=ops2; user_exc=exc2}
  =
  let t = join_expr_ty ~loc ctx t1 t2
  and ops = join_operations ops1 ops2
  and exc = join_exceptions exc1 exc2 in
  Syntax.{user_ty=t; user_ops=ops; user_exc=exc}

and meet_user_ty ~loc ctx
  Syntax.{user_ty=t1; user_ops=ops1; user_exc=exc1}
  Syntax.{user_ty=t2; user_ops=ops2; user_exc=exc2}
  =
  let t = meet_expr_ty ~loc ctx t1 t2
  and ops = meet_operations ops1 ops2
  and exc = meet_exceptions exc1 exc2 in
  Syntax.{user_ty=t; user_ops=ops; user_exc=exc}

and join_kernel_ty ~loc ctx
  Syntax.{kernel_ty=t1; kernel_ops=ops1; kernel_exc=exc1; kernel_sgn=sgn1; kernel_world=w_ty1}
  Syntax.{kernel_ty=t2; kernel_ops=ops2; kernel_exc=exc2; kernel_sgn=sgn2; kernel_world=w_ty2}
  =
  let t = join_expr_ty ~loc ctx t1 t2
  and ops = join_operations ops1 ops2
  and exc = join_exceptions exc1 exc2
  and sgn = join_signals sgn1 sgn2 in
  if not (expr_eqtype ~loc ctx w_ty1 w_ty2) then
    error ~loc (WorldTypeMismatch (w_ty1, w_ty2))
  else
    Syntax.{kernel_ty=t; kernel_ops=ops; kernel_exc=exc; kernel_sgn=sgn; kernel_world=w_ty1}

and meet_kernel_ty ~loc ctx
  Syntax.{kernel_ty=t1; kernel_ops=ops1; kernel_exc=exc1; kernel_sgn=sgn1; kernel_world=w_ty1}
  Syntax.{kernel_ty=t2; kernel_ops=ops2; kernel_exc=exc2; kernel_sgn=sgn2; kernel_world=w_ty2}
  =
  let t = meet_expr_ty ~loc ctx t1 t2
  and ops = meet_operations ops1 ops2
  and exc = meet_exceptions exc1 exc2
  and sgn = meet_signals sgn1 sgn2 in
  if not (expr_eqtype ~loc ctx w_ty1 w_ty2) then
    error ~loc (WorldTypeMismatch (w_ty1, w_ty2))
  else
    Syntax.{kernel_ty=t; kernel_ops=ops; kernel_exc=exc; kernel_sgn=sgn; kernel_world=w_ty1}


(**** Type checking ****)

let exceptions (Desugared.Exceptions excs) = Syntax.Exceptions excs

let operations (Desugared.Operations excs) = Syntax.Operations excs

let signals (Desugared.Signals excs) = Syntax.Signals excs

(** Check that a type is valid. Retrn the processed type. *)
let rec expr_ty {Location.it=t'; loc} =
  match t' with

  | Desugared.Primitive p ->
     let p =
       match p with
       | Desugared.Empty -> Syntax.Empty
       | Desugared.Bool -> Syntax.Bool
       | Desugared.Int -> Syntax.Int
       | Desugared.String -> Syntax.String
       | Desugared.Any -> Syntax.Any
     in
     Syntax.Primitive p

  | Desugared.Abstract t -> Syntax.Abstract t

  | Desugared.Alias t -> Syntax.Alias t

  | Desugared.Datatype t -> Syntax.Datatype t

  | Desugared.Product lst ->
     let lst = List.map expr_ty lst in
     Syntax.Product lst

  | Desugared.(ArrowUser (t1, t2)) ->
     let t1 = expr_ty t1
     and t2 = user_ty t2 in
     Syntax.(ArrowUser (t1, t2))

  | Desugared.(ArrowKernel (t1, t2)) ->
     let t1 = expr_ty t1
     and t2 = kernel_ty t2 in
     Syntax.(ArrowKernel (t1, t2))

  | Desugared.RunnerTy (ops1, ops2, sgns, w_ty) ->
     let ops1 = operations ops1
     and ops2 = operations ops2
     and sgns = signals sgns
     and w_ty = expr_ty w_ty in
     Syntax.RunnerTy (ops1, ops2, sgns, w_ty)

  | Desugared.ContainerTy ops ->
     let ops = operations ops in
     Syntax.ContainerTy ops

and user_ty Desugared.{user_ty; user_ops; user_exc} =
  let user_ty = expr_ty user_ty
  and user_ops = operations user_ops
  and user_exc = exceptions user_exc in
  Syntax.{user_ty; user_ops; user_exc}

and kernel_ty Desugared.{kernel_ty; kernel_ops; kernel_exc; kernel_sgn; kernel_world} =
  let kernel_ty = expr_ty kernel_ty
  and kernel_ops = operations kernel_ops
  and kernel_exc = exceptions kernel_exc
  and kernel_sgn = signals kernel_sgn
  and kernel_world = expr_ty kernel_world in
  Syntax.{kernel_ty; kernel_ops; kernel_exc; kernel_sgn; kernel_world}

(** Typecheck a datatype *)
let datatype cnstrs =
  List.map
    (function
     | (x, None) -> (x, None)
     | (x, Some t) -> (x, Some (expr_ty t)))
    cnstrs

(** Typecheck mutually recursive datatypes *)
let datatypes ctx ty_defs =
  let rec fold ctx ty_defs = function

    | [] ->
       let ty_defs = List.rev ty_defs in
       ctx, ty_defs

    | (t, cnstrs) :: lst ->
       let cnstrs = datatype cnstrs in
       let ctx = extend_datatype t cnstrs ctx in
       fold ctx ((t, cnstrs) :: ty_defs) lst
  in
  fold ctx [] ty_defs


(** Typecheck a pattern, return processed pattern and the list of identifiers
   and types bound by the pattern *)
let check_pattern ctx patt ty =
  let rec fold xts {Location.it=p'; loc} t =
    let (Normal t) = norm_ty ~loc ctx t in
    match p' with

    | Desugared.PattAnonymous ->
       Syntax.PattAnonymous, xts

    | Desugared.PattVar x ->
       Syntax.PattVar, (x, t) :: xts

    | Desugared.PattNumeral n ->
       begin match t with
       | Syntax.(Primitive Int) -> Syntax.PattNumeral n, xts
       | _ -> error ~loc (PattTypeMismatch ty)
       end

    | Desugared.PattBoolean b ->
       begin match t with
       | Syntax.(Primitive Bool) -> Syntax.PattBoolean b, xts
       | _ -> error ~loc (PattTypeMismatch ty)
       end


    | Desugared.PattQuoted s ->
       begin match t with
       | Syntax.(Primitive String) -> Syntax.PattQuoted s, xts
       | _ -> error ~loc (PattTypeMismatch ty)
       end

    | Desugared.PattConstructor (cnstr, popt) ->
       begin match t with
       | Syntax.Datatype x ->
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
       | _ -> error ~loc (PattTypeMismatch ty)
       end

    | Desugared.PattTuple ps ->
       begin match t with
       |  Syntax.Product ts ->
           let ps, xts = fold_tuple ~loc xts [] ps ts in
           Syntax.PattTuple ps, xts
       | _ -> error ~loc (PattTypeMismatch ty)
       end

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
let extend_pattern ctx p t =
  let p, xts = check_pattern ctx p t in
  let ctx = extend_idents xts ctx in
  ctx, p

(** Extend the context by typechecking the pattern against the type, also return the list
   of bound names with their types. *)
let top_extend_pattern ctx p t =
  let p, xts = check_pattern ctx p t in
  let ctx = extend_idents xts ctx in
  ctx, p, xts

(** Check that the operations [ops1] are a subset of [ops2], issue and error or a warning
   if necessary. *)
let check_operations ~fatal ~loc (Syntax.Operations ops1) (Syntax.Operations ops2) =
  if Name.Set.subset ops1 ops2 then
    error ~loc (UnhandledOperations (Name.Set.diff ops2 ops1))
  else
    warning ~loc (UnhandledOperations (Name.Set.diff ops2 ops1))

(** Check that the exceptions [exc1] are a subset of [exc2], issue and error or a warning
   if necessary. *)
let check_exceptions ~fatal ~loc (Syntax.Exceptions exc1) (Syntax.Exceptions exc2) =
  if Name.Set.subset exc1 exc2 then
    error ~loc (UnhandledExceptions (Name.Set.diff exc2 exc1))
  else
    warning ~loc (UnhandledExceptions (Name.Set.diff exc2 exc1))

(** Check that the signals [sgn1] are a subset of [sgn2], issue and error or a warning
   if necessary. *)
let check_signals ~fatal ~loc (Syntax.Signals sgn1) (Syntax.Signals sgn2) =
  if Name.Set.subset sgn1 sgn2 then
    error ~loc (UnhandledSignals (Name.Set.diff sgn2 sgn1))
  else
    warning ~loc (UnhandledSignals (Name.Set.diff sgn2 sgn1))


(** [infer_expr ctx e] infers the expression type [ty] of an expression [e]. It
   returns the processed expression [e] and its type [ty]. *)
let rec infer_expr (ctx : context) {Location.it=e'; loc} =
  let locate = Location.locate ~loc in
  match e' with
  | Desugared.Var x ->
     let k, ty = lookup ~loc x ctx in
     locate (Syntax.Var k), ty

  | Desugared.ExprAscribe (e, t) ->
     let t = expr_ty t in
     let e = check_expr ctx e t in
     e, t

  | Desugared.Numeral n ->
     locate (Syntax.Numeral n), Syntax.(Primitive Int)

  | Desugared.Boolean b ->
     locate (Syntax.Boolean b), Syntax.(Primitive Bool)

  | Desugared.Quoted s ->
     locate (Syntax.Quoted s), Syntax.(Primitive String)

  | Desugared.Constructor (cnstr, eopt) ->
     let ty, topt = lookup_constructor ~loc cnstr ctx in
     let e = check_constructor ~loc ctx cnstr eopt topt in
     locate (Syntax.Constructor (cnstr, e)), Syntax.Datatype ty

  | Desugared.Tuple lst ->
     let lst = List.map (infer_expr ctx) lst in
     locate (Syntax.Tuple (List.map fst lst)),  Syntax.Product (List.map snd lst)

  | Desugared.FunUser ((p, Some t), c) ->
     let t = expr_ty t in
     let ctx, p = extend_pattern ctx p t in
     let c, c_ty = infer_user ctx c in
     locate (Syntax.FunUser (p, c)), Syntax.ArrowUser (t, c_ty)

  | Desugared.FunUser ((p, None), _) ->
     error ~loc:p.Location.loc CannotInferArgument

  | Desugared.FunKernel ((p, Some t), c) ->
     let t = expr_ty t in
     let ctx, p = extend_pattern ctx p t in
     let c, c_ty = infer_kernel ctx c in
     locate (Syntax.FunKernel (p, c)), Syntax.ArrowKernel (t, c_ty)

  | Desugared.FunKernel ((p, None), _) ->
     error ~loc:p.Location.loc CannotInferArgument

  | Desugared.Runner (coops, w_ty) ->
     let w_ty = expr_ty w_ty in
     let coops, ops1, ops2, sgns, w_ty = infer_coops ~loc ctx w_ty coops in
     locate (Syntax.Runner coops), Syntax.RunnerTy (ops1, ops2, sgns, w_ty)

(** [check_expr ctx e ty] checks that expression [e] has type [ty] in context [ctx].
    It returns the processed expression [e]. *)
and check_expr (ctx : context) ({Location.it=e'; loc} as e) ty =
  let locate = Location.locate ~loc in
  match e', norm_ty ~loc ctx ty with

  (* Synthesizing terms and [any] type *)
  | _, Normal Syntax.(Primitive Any)
  | Desugared.(Quoted _ | Numeral _ | Boolean _ | Constructor _ | FunUser ((_, Some _), _) |
                Var _ | ExprAscribe _ | Runner _ ), _ ->
     let e, ty' = infer_expr ctx e in
     if expr_subty ~loc ctx ty' ty
     then
       e
     else
       error ~loc (ExprTypeMismatch (ty, ty'))

  | Desugared.FunUser ((p, None), e), ty' ->
     begin
       match as_user_arrow ty' with
       | Some (t, u) ->
          let ctx, p = extend_pattern ctx p t in
          let c = check_user ctx e u in
          locate (Syntax.FunUser (p, c))
       | None ->
          error ~loc (TypeExpectedButFunction ty)
     end

  | Desugared.FunKernel ((p, t_opt), e), ty' ->
     begin
       match as_kernel_arrow ty' with
       | None ->
          error ~loc (TypeExpectedButFunction ty)
       | Some (t, u) ->
          let t =
            match t_opt with
            | None -> t
            | Some ({Location.loc;_} as t') ->
               let t' = expr_ty t' in
               if expr_subty ~loc ctx t t' then t' else error ~loc (ExprTypeMismatch (t, t'))
          in
          let ctx, p = extend_pattern ctx p t in
          let c = check_kernel ctx e u in
          locate (Syntax.FunKernel (p, c))
     end

  | Desugared.Tuple es, ty' ->
     begin
       match as_product ty' with
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

  | None, Some _ ->
     error ~loc (IllegalConstructor cnstr)

  | Some _, None ->
     error ~loc (IllegalConstructor cnstr)

(** [infer_user ctx c] infers the type [ty] of a user computation [c]. It returns
    the processed computation [c] and its type [ty].  *)
and infer_user (ctx : context) {Location.it=c'; loc} =
  let locate = Location.locate ~loc in
  match c' with

  | Desugared.UserAscribe (c, t) ->
     let t = user_ty t in
     let c = check_user ctx c t in
     c, t

  | Desugared.UserVal e ->
     let e, e_ty = infer_expr ctx e in
     locate (Syntax.UserVal e), Syntax.pure_user_ty e_ty

  | Desugared.UserEqual (e1, e2) ->
     let e1, t = infer_expr ctx e1 in
     let e2 = check_expr ctx e2 t in
     locate (Syntax.UserEqual (e1, e2)), Syntax.(pure_user_ty (Primitive Bool))

  | Desugared.UserTry (c, hnd) ->
     let c, Syntax.{user_ty; user_ops; user_exc} = infer_user ctx c in
     let hnd, hnd_ty = infer_user_handler ctx user_ty user_exc hnd in
     let t = Syntax.pollute_user hnd_ty user_ops Syntax.empty_exceptions in
     locate (Syntax.UserTry (c, hnd)), t

  | Desugared.UserLet (p, c1, c2) ->
     let c1, (Syntax.{user_ty=t1'; user_ops=ops1; user_exc=exc1} as t1) = infer_user ctx c1 in
     let ctx, p = extend_pattern ctx p t1' in
     let c2, t2 = infer_user ctx c2 in
     let t2 = Syntax.pollute_user t2 ops1 exc1 in
     locate (Syntax.UserLet (p, c1, c2)), t2

  | Desugared.UserLetRec (fs, c) ->
     let ctx, pcs, _fts = infer_letrec ctx fs in
     let c, c_ty = infer_user ctx c in
     locate (Syntax.UserLetRec (pcs, c)), c_ty

  | Desugared.UserMatch (e, lst) ->
     let e, e_ty = infer_expr ctx e in
     let lst, ty = infer_user_match_clauses ~loc ctx e_ty lst in
     locate (Syntax.UserMatch (e, lst)), ty

  | Desugared.UserApply (e1, e2) ->
     let e1, t1 = infer_expr ctx e1 in
     begin match as_user_arrow (norm_ty ~loc:(e1.Location.loc) ctx t1) with
       | Some (u1, u2) ->
          let e2 = check_expr ctx e2 u1 in
          locate (Syntax.UserApply (e1, e2)), u2
       | None ->
          error ~loc:(e1.Location.loc) (FunctionExpected t1)
     end

  | Desugared.UserOperation (op, e) ->
     let ty1, ty2, exc_ops = lookup_operation ~loc op ctx in
     let e = check_expr ctx e ty1 in
     let e_ty = Syntax.operation_user_ty ty2 op in
     locate (Syntax.(UserOperation (op, e, exc_ops))), e_ty

  | Desugared.UserRaise (exc, e) ->
     let e_ty = lookup_exception ~loc exc ctx in
     let e = check_expr ctx e e_ty in
     let ty = Syntax.raise_user_ty exc in
     locate (Syntax.UserRaise (exc, e)), ty

  | Desugared.UserUsing (rnr, w, c, fin) ->
     (* The runner [rnr] handles operations [ops1], triggers operations [ops2] and
        signals [sgn]. It operates on a world of type [w_ty]. It also triggers
        exceptions that the [ops1] allow. *)
     let rnr, (ops1, ops2, sgn, w_ty) = infer_runner ctx rnr in
     (* Check that the world [w] has at most the type [w_ty], which is ok because
        we use [w] covariantly (to insert it into the state). *)
     let w = check_expr ctx w w_ty in
     (* infer the type of the body [c] *)
     let c, Syntax.{user_ty=c_ty; user_ops=c_ops; user_exc=c_exc} = infer_user ctx c in
     (* the finally clause [fin] raises exceptions [fin_excs], signals [fin_sgs], and
        evaluates to user computations of type [fin_ty] (thus the information about
        [fin_exc] and [fin_sgn] is already contained in [fin_ty] *)
     let fin, fin_exc, fin_sgn, fin_ty = infer_finally ~loc ctx c_ty w_ty fin in
     (* check that finally intercepts the exceptions of [c] *)
     check_exceptions ~fatal:true ~loc c_exc fin_exc ;
     (* check that the runner intercepts the operations of [c] *)
     check_operations ~fatal:true ~loc c_ops ops1 ;
     (* check that the finally intercepts the signals of the runner [rnr] *)
     check_signals ~fatal:true ~loc sgn fin_sgn ;
     locate (Syntax.UserUsing (rnr, w, c, fin)), fin_ty

  | Desugared.UserExec (c, w, fin) ->
     let w, w_ty = infer_expr ctx w in
     let c, Syntax.{kernel_ty=c_ty; kernel_ops=c_ops;
                    kernel_exc=c_exc; kernel_sgn=c_sgn; kernel_world=_} =
       infer_kernel ~world:w_ty ctx c
     in
     let fin, fin_exc, fin_sgn, fin_ty = infer_finally ~loc ctx c_ty w_ty fin in
     check_exceptions ~fatal:true ~loc c_exc fin_exc ;
     check_signals ~fatal:true ~loc c_sgn fin_sgn ;
     locate (Syntax.UserExec (c, w, fin)), fin_ty

and infer_kernel ?world (ctx : context) {Location.it=c'; loc} =
  let get_world () =
     match world with
     | None -> error ~loc CannotInferWorld
     | Some w_ty -> w_ty
  in
  let locate = Location.locate ~loc in
  match c' with

  | Desugared.KernelAscribe (c, t) ->
     let (Syntax.{kernel_world;_} as t) = kernel_ty t in
     begin match world with
     | None -> ()
     | Some w_ty ->
        if not (expr_eqtype ~loc ctx kernel_world w_ty) then
          error ~loc (WorldTypeMismatch (w_ty, kernel_world))
     end ;
     let c = check_kernel ctx c t in
     c, t

  | Desugared.KernelVal e ->
     let w_ty = get_world ()
     and e, e_ty = infer_expr ctx e in
     locate (Syntax.KernelVal e), Syntax.pure_kernel_ty e_ty w_ty

  | Desugared.KernelEqual (e1, e2) ->
     let w_ty = get_world () in
     let e1, t = infer_expr ctx e1 in
     let e2 = check_expr ctx e2 t in
     locate (Syntax.KernelEqual (e1, e2)), Syntax.(pure_kernel_ty (Primitive Bool) w_ty)

  | Desugared.KernelTry (c, hnd) ->
     let c, Syntax.{kernel_ty; kernel_ops; kernel_exc; kernel_sgn; kernel_world=world} = infer_kernel ?world ctx c in
     let hnd, hnd_ty = infer_kernel_handler ~world ctx kernel_ty kernel_exc hnd in
     let t = Syntax.pollute_kernel hnd_ty kernel_ops Syntax.empty_exceptions kernel_sgn in
     locate (Syntax.KernelTry (c, hnd)), t

  | Desugared.KernelLet (p, c1, c2) ->
     let c1, (Syntax.{kernel_ty=t1'; kernel_ops=ops1; kernel_exc=exc1; kernel_sgn=sgn1; kernel_world=world} as t1) =
       infer_kernel ?world ctx c1
     in
     let ctx, p = extend_pattern ctx p t1' in
     let c2, t2 = infer_kernel ~world ctx c2 in
     let t2 = Syntax.pollute_kernel t2 ops1 exc1 sgn1 in
     locate (Syntax.KernelLet (p, c1, c2)), t2

  | Desugared.KernelLetRec (fs, c) ->
     let ctx, pcs, _fts = infer_letreck ctx fs in
     let c, c_ty = infer_kernel ?world ctx c in
     locate (Syntax.KernelLetRec (pcs, c)), c_ty

  | Desugared.KernelMatch (e, lst) ->
     let e, e_ty = infer_expr ctx e in
     let lst, ty = infer_kernel_match_clauses ?world ~loc ctx e_ty lst in
     locate (Syntax.KernelMatch (e, lst)), ty

  | Desugared.KernelApply (e1, e2) ->
     let e1, t1 = infer_expr ctx e1 in
     begin match as_kernel_arrow (norm_ty ~loc:(e1.Location.loc) ctx t1) with
       | Some (u1, u2) ->
          let e2 = check_expr ctx e2 u1 in
          locate (Syntax.KernelApply (e1, e2)), u2
       | None ->
          error ~loc:(e1.Location.loc) (FunctionExpected t1)
     end

  | Desugared.KernelOperation (op, e) ->
     let w_ty = get_world () in
     let ty1, ty2, exc_ops = lookup_operation ~loc op ctx in
     let e = check_expr ctx e ty1 in
     let e_ty = Syntax.operation_kernel_ty ty2 op w_ty in
     locate (Syntax.(KernelOperation (op, e, exc_ops))), e_ty

  | Desugared.KernelRaise (exc, e) ->
     let w_ty = get_world () in
     let e_ty = lookup_exception ~loc exc ctx in
     let e = check_expr ctx e e_ty in
     let ty = Syntax.raise_kernel_ty exc w_ty in
     locate (Syntax.KernelRaise (exc, e)), ty

  | Desugared.KernelKill (sgn, e) ->
     let w_ty = get_world () in
     let e_ty = lookup_signal ~loc sgn ctx in
     let e = check_expr ctx e e_ty in
     let ty = Syntax.kill_ty sgn w_ty in
     locate (Syntax.KernelKill (sgn, e)), ty

  | Desugared.KernelGetenv ->
     let w_ty = get_world () in
     let t = Syntax.pure_kernel_ty w_ty w_ty in
     locate Syntax.KernelGetenv, t

  | Desugared.KernelSetenv e ->
     let w_ty = get_world () in
     let e = check_expr ctx e w_ty in
     let t = Syntax.pure_kernel_ty Syntax.unit_ty w_ty in
     locate (Syntax.KernelSetenv e), t

  | Desugared.KernelExec (c, hnd) ->
     let c, Syntax.{user_ty=c_ty; user_ops=c_ops; user_exc=c_exc} =
       infer_user ctx c
     in
     let hnd, hnd_ty = infer_kernel_handler ?world ctx c_ty c_exc hnd in
     let t = Syntax.pollute_kernel hnd_ty c_ops Syntax.empty_exceptions Syntax.empty_signals in
     locate (Syntax.KernelExec (c, hnd)), t

(** Infer the type of an exception handler, where [ty_val] is the type of the argument for
   the [val] case, and [exc] are the exceptions that should be handled. It supplements the
   handler with missing exception cases by inserting default handlers for those *)
and infer_user_handler ctx ty_val exc Desugared.{exc_val=(px,c_val); exc_raise} =
  let px, c_val, ty =
    let ctx, px = extend_binder ctx px ty_val in
    let c_val, ty = infer_user ctx c_val in
    (px, c_val, ty)
  in
  let exc_raise =
    List.map
      (fun (exc, px, c) ->
        let t = (??) in (* lookup exc here *)
       let ctx, px = extend_binder ctx px t in
       let c = check_user ctx c ty in
       (exc, px, c))
      exc_raise
    (* attach missing cases somewhere *)
  in
  Syntax.{exc_val = (px, c_val); exc_raise}, ty

and infer_kernel_handler ?world ctx ty_val exc Desugared.{exc_val=(px,c_val); exc_raise} =
  let px, c_val, ty =
    let ctx, px = extend_binder ctx px ty_val in
    let c_val, ty = infer_kernel ?world ctx c_val in
    (px, c_val, ty)
  in
  let exc_raise =
    List.map
      (fun (exc, px, c) ->
        let t = (??) in (* lookup exc here *)
       let ctx, px = extend_binder ctx px t in
       let c = check_kernel ctx c ty in
       (exc, px, c))
      exc_raise
    (* attach missing cases somewhere *)
  in
  Syntax.{exc_val = (px, c_val); exc_raise}, ty

(** Infer user [let rec] *)
and infer_letrec ctx fs =
  let ctx, fts =
    List.fold_left
      (fun (ctx, fts) (f, t, _, u, _) ->
        let t = user_ty t
        and u = expr_ty u in
        extend_ident f (Syntax.ArrowUser (u, t)) ctx,
        (f, u, t) :: fts)
      (ctx, [])
      fs
  in
  let pcs =
    List.map2
    (fun (_, _, p, _, c) (_, u, t) ->
      let ctx, p = extend_pattern ctx p u in
      let c = check_user ctx c t in
      (p, c))
    fs fts
  in
  ctx, pcs, fts

(* Infer kernel [let reck] *)
and infer_letreck ctx fs =
  let ctx, fts =
    List.fold_left
      (fun (ctx, fts) (f, t, _, u, _) ->
        let t = kernel_ty t
        and u = expr_ty u in
        extend_ident f (Syntax.ArrowKernel (u, t)) ctx,
        (f, u, t) :: fts)
      (ctx, [])
      fs
  in
  let pcs =
    List.map2
    (fun (_, _, p, _, c) (_, u, t) ->
      let ctx, p = extend_pattern ctx p u in
      let c = check_kernel ctx c t in
      (p, c))
    fs fts
  in
  ctx, pcs, fts

and infer_user_match_clauses ~loc ctx patt_ty lst =
  match lst with
  | [] -> error ~loc CannotInferMatch
  | (p, c) :: lst ->
     let ctx, p = extend_binder ctx p patt_ty in
     let c, c_ty = infer_user ctx c in
     let rec fold clauses ty = function
       | [] -> List.rev clauses, ty
       | (p, c) :: lst ->
          let ctx, p = extend_binder ctx p patt_ty in
          let c, c_ty = infer_user ctx c in
          let ty = join_user_ty ~loc:c.Location.loc ctx c_ty ty in
          fold ((p,c) :: clauses) ty lst
     in
     let clauses, ty = fold [] c_ty lst in
     ((p, c) :: clauses), ty

and infer_kernel_match_clauses ~loc ctx ?world patt_ty lst =
  match lst with
  | [] -> error ~loc CannotInferMatch
  | (p, c) :: lst ->
     let ctx, p = extend_binder ctx p patt_ty in
     let c, c_ty = infer_kernel ctx ?world c in
     let rec fold clauses ty = function
       | [] -> List.rev clauses, ty
       | (p, c) :: lst ->
          let ctx, p = extend_binder ctx p patt_ty in
          let c, c_ty = infer_kernel ctx ?world c in
          let ty = join_kernel_ty ~loc:c.Location.loc ctx c_ty ty in
          fold ((p,c) :: clauses) ty lst
     in
     let clauses, ty = fold [] c_ty lst in
     ((p, c) :: clauses), ty

and infer_coops ~loc ctx w_ty lst =
  let rec fold coops ops1 ops2 sgns = function

    | [] ->
       let coops = List.rev coops in
       coops, ops1, ops2, sgns

    | (op, px, c) :: lst ->
       if Name.Set.mem op ops1 then
         error ~loc (DuplicateOperation op)
       else
         let (x_ty, op_ty, op_exc) = lookup_operation ~loc op ctx in
         let c, Syntax.{kernel_ty=c_ty'; kernel_ops=c_ops; kernel_exc=c_exc; kernel_sgn=c_sgn; kernel_world=_} =
           let ctx, px = extend_binder ctx px x_ty in
           infer_kernel ~world:w_ty ctx c
         in
         check_exceptions ~loc ~fatal:true c_exc op_exc ;

         if not (expr_subty ~loc ctx c_ty op_ty) then
           error ~loc (CoopTypeMismatch (c_ty'', c_ty')) ;
         let coops = (op, px, pw, c) :: coops
         and ops1 = Name.Set.add op ops1
         and ops2 = Name.Set.union
         and sgn2 = join_signature sgn2 c_sgn in
         fold coops ops sgn2 lst
  in
  fold [] Name.Set.empty Name.Set.empty Name.Set.empty lst

and infer_runner ctx rnr =
  let e, e_ty = infer_expr ctx rnr in
  match as_runner (norm_ty ~loc:rnr.Location.loc ctx e_ty) with

    | Some (ops1, ops2, sgn, w_ty) ->
       e, (ops1, ops2, sgn, w_ty)

    | None ->
       error ~loc:rnr.Location.loc (RunnerExpected e_ty)

(** Infer the types of a finally clause, for given type of [val] case [x_ty] and world type [w_ty]. *)
and infer_finally ~loc ctx x_ty w_ty Desugared.{fin_val; fin_raise; fin_kill} =
  let fin_val, ty_val =
    let (px, pw, c_val) = fin_val in
    let ctx, px = extend_binder ctx px x_ty in
    let ctx, pw = extend_binder ctx pw w_ty in
    let c_val, ty_val = infer_user ctx c_val in
    (px, pw, c_val), ty_val
  in
  let fin_raise, fin_excs, fin_ty =
    let rec fold fs excs ty = function
      | [] ->
         let fs = List.rev fs in
         fs, excs, ty
      | (exc, px, pw, c_exc) :: lst ->
         begin
           if List.exists (fun (exc', _, _, _) -> Name.equal exc exc') fs then error ~loc (DuplicateException exc) ;
           let u_ty = lookup_exception ~loc exc ctx in
           let ctx, px = extend_binder ctx px u_ty in
           let ctx, pw = extend_binder ctx pw w_ty in
           let c_exc, exc_ty = infer_user ctx c_exc in
           let ty = join_user_ty ~loc:c_exc.Location.loc ctx ty exc_ty in
           let excs = Name.Set.add exc excs in
           fold ((exc, px, pw, c_exc) :: fs) excs ty lst
         end
    in
    fold [] Name.Set.empty ty_val fin_raise
  in
  let fin_kill, fin_sgns, fin_ty =
    let rec fold fs sgs ty = function
      | [] ->
         let fs = List.rev fs in
         fs, sgs, ty
      | (sg, px, c_sg) :: lst ->
         begin
           if List.exists (fun (sg', _, _) -> Name.equal sg sg') fs then error ~loc (DuplicateSignal sg) ;
           let u_ty = lookup_signal ~loc sg ctx in
           let ctx, px = extend_binder ctx px u_ty in
           let c_sg, sg_ty = infer_user ctx c_sg in
           let ty = join_user_ty ~loc:c_sg.Location.loc ctx ty sg_ty in
           let sgs = Name.Set.add sg sgs in
           fold ((sg, px, c_sg) :: fs) sgs ty lst
         end
    in
    fold [] Name.Set.empty fin_ty fin_kill
  in
  Syntax.{fin_val; fin_kill; fin_raise},
  Syntax.Exceptions fin_excs,
  Syntax.Signals fin_sgns,
  fin_ty


and extend_binder ctx (p, topt) t =
  let loc = p.Location.loc in
  match topt with
  | None -> extend_pattern ctx p t
  | Some t' ->
     let t' = expr_ty t' in
     if not (expr_subty ~loc ctx t t') then
       error ~loc (ExprTypeMismatch (t, t')) ;
     extend_pattern ctx p t'


(** [check_user ctx c ty] checks that computation [c] has user type [ty] in context [ctx].
    It returns the processed computation [c]. *)
and check_user ctx ({Location.it=c'; loc} as c) check_ty =
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
     let c1, (Syntax.{comp_ty=c1_ty';_} as c1_ty) = infer_user ctx c1 in
     check_dirt ~fatal:true ~loc c1_ty check_sgn ;
     let ctx, p = extend_pattern ctx p c1_ty' in
     let c2 = check_comp ctx c2 check_ty in
     locate (Syntax.Let (p, c1, c2))

  | Desugared.LetRec (fs, c) ->
     let ctx, pcs, _ = infer_rec ctx fs in
     let c = check_comp ctx c check_ty in
     locate (Syntax.LetRec (pcs, c))

  | (Desugared.Equal _ | Desugared.Apply _ | Desugared.AscribeComp _ |
     Desugared.Operation _ | Desugared.Signal _ | Desugared.Run _ | Desugared.Try _) ->
     let c, c_ty = infer_user ctx c in
     if comp_subty ~loc ctx c_ty check_ty
     then
       c
     else
       error ~loc (CompTypeMismatch (check_ty, c_ty))


and check_kernel ctx c t =
  (??)

and check_match_clauses ctx patt_ty ty lst =
  List.map (check_match_clause ctx patt_ty ty) lst

and check_match_clause ctx patt_ty ty (p, c) =
  let ctx, p = extend_binder ctx p patt_ty in
  let c = check_comp ctx c ty in
  (p, c)

let top_infer_user ctx c =
  let ops = lookup_container ctx in
  let c, c_ty = infer_user ctx c in
  check_dirt ~fatal:false ~loc:c.Location.loc c_ty Syntax.{sig_ops=ops; sig_sgs=Name.Set.empty} ;
  c, c_ty

let rec toplevel ~quiet ctx {Location.it=d'; loc} =
  let ctx, d' =
    match d' with

    | Desugared.TopLoad lst ->
       let ctx, lst = topfile ~quiet ctx lst in
       ctx, Syntax.TopLoad lst

    | Desugared.TopLet (p, c) ->
       let c, Syntax.{comp_ty=c_ty';_} = top_infer_user ctx c in
       let ctx, p, xts = top_extend_pattern ctx p c_ty' in
       ctx, Syntax.TopLet (p, xts, c)

    | Desugared.TopLetRec fs ->
       let ctx, pcs, fts = infer_rec ctx fs in
       let fts = List.map (fun (f, u, t) -> (f, Syntax.Arrow (u, t))) fts in
       ctx, Syntax.TopLetRec (pcs, fts)

    | Desugared.TopContainer c ->
       begin
         let c, Syntax.{comp_ty=c_ty';_} = top_infer_user ctx c in
         match as_container (norm_ty ~loc ctx c_ty') with
         | None -> error ~loc (ContainerExpected c_ty')
         | Some ops ->
            let ctx = set_container ops ctx in
            ctx, Syntax.TopContainer (c, ops)
       end

    | Desugared.TopComp c ->
       let c, Syntax.{comp_ty=c_ty'; _} = top_infer_user ctx c in
       ctx, Syntax.TopComp (c, c_ty')

    | Desugared.DefineAlias (t, abbrev) ->
       let abbrev = expr_ty abbrev in
       let ctx = extend_alias t abbrev ctx in
       ctx, Syntax.DefineAlias (t, abbrev)

    | Desugared.DefineAbstract t ->
       ctx, Syntax.DefineAbstract t

    | Desugared.DefineDatatype ty_defs ->
       let ctx, ty_defs = datatypes ctx ty_defs in
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
