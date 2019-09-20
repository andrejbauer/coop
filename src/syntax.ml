(** Type-checked abstract syntax of Coop. *)

type operations = Operations of Name.Set.t

type exceptions = Exceptions of Name.Set.t

type signals = Signals of Name.Set.t

(** Primitive types *)
type primitive =
  | Empty
  | Int
  | Bool
  | String
  | Any

(** Types of expressions *)
type expr_ty =
  | Abstract of Name.t
  | Alias of Name.t
  | Datatype of Name.t
  | Primitive of primitive
  | Product of expr_ty list
  | ArrowUser of expr_ty * user_ty
  | ArrowKernel of expr_ty * kernel_ty
  | RunnerTy of runner_ty
  | ContainerTy of operations

(** The typing information for a user computation *)
and user_ty =
  { user_ty : expr_ty
  ; user_ops : operations
  ; user_exc : exceptions }

(** The typing information for a kernel computation *)
and kernel_ty =
  { kernel_ty : expr_ty
  ; kernel_ops : operations
  ; kernel_exc : exceptions
  ; kernel_sgn : signals
  ; kernel_world : expr_ty }

(** Runner *)
and runner_ty = operations * operations * signals * expr_ty

(** The body of a datatype definition *)
type datatype = (Name.t * expr_ty option) list

(** Patterns *)
type pattern =
  | PattAnonymous
  | PattVar
  | PattNumeral of int
  | PattBoolean of bool
  | PattQuoted of string
  | PattConstructor of Name.t * pattern option
  | PattTuple of pattern list

(** De Bruijn index *)
type index = int

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | Var of index
  | Numeral of int
  | Boolean of bool
  | Quoted of string
  | Constructor of Name.t * expr option
  | Tuple of expr list
  | FunUser of pattern * user
  | FunKernel of pattern * kernel
  | Runner of runner_clause list

(** User computations *)
and user = user' Location.located
and user' =
  | UserVal of expr
  | UserEqual of expr * expr
  | UserTry of user * user exception_handler
  | UserLet of pattern * user * user
  | UserLetRec of user rec_clause list * user
  | UserApply of expr * expr
  | UserMatch of expr * (pattern * user) list
  | UserOperation of Name.t * expr * exceptions
  | UserRaise of Name.t * expr
  | UserUsing of expr * expr * user * finally
  | UserExec of kernel * expr * finally

(** Kernel computations *)
and kernel = kernel' Location.located
and kernel' =
  | KernelVal of expr
  | KernelEqual of expr * expr
  | KernelTry of kernel * kernel exception_handler
  | KernelLet of pattern * kernel * kernel
  | KernelLetRec of kernel rec_clause list * kernel
  | KernelApply of expr * expr
  | KernelMatch of expr * (pattern * kernel) list
  | KernelOperation of Name.t * expr * exceptions
  | KernelRaise of Name.t * expr
  | KernelKill of Name.t * expr
  | KernelGetenv
  | KernelSetenv of expr
  | KernelExec of user * kernel exception_handler

and finally =
  { fin_val : pattern * pattern * user
  ; fin_raise : (Name.t * pattern * pattern * user) list
  ; fin_kill : (Name.t * pattern * user) list }

(** Exception handler *)
and 'a exception_handler =
  { exc_val : pattern * 'a
  ; exc_raise : (Name.t * pattern * 'a) list }

and runner_clause = Name.t * pattern * kernel

and 'a rec_clause = pattern * 'a

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * (Name.t * expr_ty) list * user
  | TopLetRec of user rec_clause list * (Name.t * expr_ty) list
  | TopContainer of user * operations
  | TopUser of user * expr_ty
  | DefineAbstract of Name.t
  | DefineAlias of Name.t * expr_ty
  | DefineDatatype of (Name.t * datatype) list
  | DeclareOperation of Name.t * expr_ty * expr_ty
  | DeclareException of Name.t * expr_ty
  | DeclareSignal of Name.t * expr_ty
  | External of Name.t * expr_ty * string

(** The unit type *)
let unit_ty = Product []

(** Empty sets of gadgets *)

let empty_operations = Operations Name.Set.empty
let empty_exceptions = Exceptions Name.Set.empty
let empty_signals = Signals Name.Set.empty

(** Make a pure user-computation type *)
let pure_user_ty t =
 { user_ty = t
 ; user_ops = empty_operations
 ; user_exc = empty_exceptions }

(** Make a pure kernel-computation type *)
let pure_kernel_ty t tw =
  { kernel_ty = t
  ; kernel_ops = empty_operations
  ; kernel_exc = empty_exceptions
  ; kernel_sgn = empty_signals
  ; kernel_world = tw }

(** Pollute a user type with given operations and exceptions *)
let pollute_user {user_ty; user_ops=Operations ops; user_exc=Exceptions exc}
                 (Operations ops') (Exceptions exc') =
  { user_ty
  ; user_ops = Operations (Name.Set.union ops ops')
  ; user_exc = Exceptions (Name.Set.union exc exc') }

(** Pollute a kernel type with given operations, exceptions, and signals *)
let pollute_kernel {kernel_ty; kernel_ops=Operations ops; kernel_exc=Exceptions exc; kernel_sgn=Signals sgn; kernel_world}
                 (Operations ops') (Exceptions exc') (Signals sgn') =
  { kernel_ty
  ; kernel_ops = Operations (Name.Set.union ops ops')
  ; kernel_exc = Exceptions (Name.Set.union exc exc')
  ; kernel_sgn = Signals (Name.Set.union sgn sgn')
  ; kernel_world }

(** The user type of the given operation [op] *)
let operation_user_ty t op =
  { user_ty = t
  ; user_ops = Operations (Name.Set.add op Name.Set.empty)
  ; user_exc = empty_exceptions }

(** The kernel type of the given operation [op] *)
let operation_kernel_ty t op tw =
  { kernel_ty = t
  ; kernel_ops = Operations (Name.Set.add op Name.Set.empty)
  ; kernel_exc = empty_exceptions
  ; kernel_sgn = empty_signals
  ; kernel_world = tw }

(** The user type of a raise *)
let raise_user_ty exc =
  { user_ty = Primitive Any
  ; user_ops = empty_operations
  ; user_exc = Exceptions (Name.Set.add exc Name.Set.empty) }

(** The kernel type of a raise *)
let raise_kernel_ty exc w_ty =
  { kernel_ty = Primitive Any
  ; kernel_ops = empty_operations
  ; kernel_exc = Exceptions (Name.Set.add exc Name.Set.empty)
  ; kernel_sgn = empty_signals
  ; kernel_world = w_ty }

(** The kernel type of a kill *)
let kill_ty sgn w_ty =
  { kernel_ty = Primitive Any
  ; kernel_ops = empty_operations
  ; kernel_exc = empty_exceptions
  ; kernel_sgn = Signals (Name.Set.add sgn Name.Set.empty)
  ; kernel_world = w_ty }

(** Pretty-print a primitive type *)
let print_primitive p ppf =
  Format.fprintf ppf
  (match p with
  | Empty -> "empty"
  | Int -> "int"
  | Bool -> "bool"
  | String -> "string"
  | Any -> "any")

(** Pretty-print an expresion type *)
let rec print_expr_ty ?max_level ty ppf =
  match ty with

  | Abstract t -> Format.fprintf ppf "%t" (Name.print t)

  | Alias t -> Format.fprintf ppf "%t" (Name.print t)

  | Datatype t -> Format.fprintf ppf "%t" (Name.print t)

  | Primitive p -> print_primitive p ppf

  | Product [] -> Format.fprintf ppf "unit"

  | Product lst ->
     let st = " " ^ Print.char_times () in
     Print.print ?max_level ~at_level:Level.product ppf "%t"
       (Print.sequence (print_expr_ty ~max_level:Level.product_arg) st lst)

  | ArrowUser (t1, {user_ty=t2; user_ops=Operations ops; user_exc=Exceptions excs}) ->
     Print.print ?max_level ~at_level:Level.arr ppf "%t@ %t@ %t%t"
       (print_expr_ty ~max_level:Level.arr_left t1)
       (print_arrow ops)
       (print_expr_ty ~max_level:Level.arr_right t2)
       (print_exceptions ~empty:false excs)

  | ArrowKernel (t1, {kernel_ty=t2; kernel_ops=Operations ops; kernel_exc=Exceptions excs; kernel_sgn=Signals sgns; kernel_world=wt}) ->
     Print.print ?max_level ~at_level:Level.arr ppf "%t@ %t@ %t%t%t@%t"
       (print_expr_ty ~max_level:Level.arr_left t1)
       (print_arrow ops)
       (print_expr_ty ~max_level:Level.arr_right t2)
       (print_exceptions ~empty:false excs)
       (print_signals ~empty:false sgns)
       (print_expr_ty ~max_level:Level.world_ty wt)

  | RunnerTy rnr_ty -> print_runner_ty rnr_ty ppf

  | ContainerTy (Operations ops) ->
     Format.fprintf ppf "{%t}"
       (Print.names ops)

and print_exceptions ~empty excs ppf =
  if Name.Set.is_empty excs then
    (if empty then Format.fprintf ppf "[}")
  else
    Format.fprintf ppf "!{%t}" (Print.names excs)

and print_operations ~empty ops ppf =
  if Name.Set.is_empty ops then
    (if empty then Format.fprintf ppf "{}")
  else
    Format.fprintf ppf "{%t}" (Print.names ops)

and print_signals ~empty sgns ppf =
  if Name.Set.is_empty sgns then
    (if empty then Format.fprintf ppf "{}")
  else
    Format.fprintf ppf "%s{%t}" (Print.char_lightning ()) (Print.names sgns)

and print_arrow ops ppf =
  if Name.Set.is_empty ops then
    Format.fprintf ppf "%s" (Print.char_arrow ())
  else
    Format.fprintf ppf "%s%t%s"
      (Print.char_prearrow ())
      (Print.names ops)
      (Print.char_postarrow ())

and print_user_ty ?max_level {user_ty=t; user_ops=Operations ops; user_exc=Exceptions excs} ppf =
    Print.print ?max_level ~at_level:Level.user_ty ppf "%t@ %t%t"
      (print_operations ~empty:false ops)
      (print_expr_ty ~max_level:Level.world_ty t)
      (print_exceptions ~empty:false excs)

and print_kernel_ty ?max_level {kernel_ty=t; kernel_ops=Operations ops; kernel_exc=Exceptions excs; kernel_sgn=Signals sgns; kernel_world=wt} ppf =
    Print.print ?max_level ~at_level:Level.kernel_ty ppf "%t@ %t%t@%t"
      (print_operations ~empty:false ops)
      (print_expr_ty ~max_level:Level.user_ty_left t)
      (print_exceptions ~empty:false excs)
      (print_expr_ty ~max_level:Level.world_ty wt)

and print_runner_ty (Operations ops1, Operations ops2, Signals sgns, wt) ppf =
  Format.fprintf ppf "%t@ %s@ %t%t%t"
    (print_operations ~empty:true ops1)
    (Print.char_darrow ())
    (print_operations ~empty:true ops2)
    (print_signals ~empty:false sgns)
    (print_expr_ty ~max_level:Level.runner_ty_world wt)

and print_container_ty (Operations ops) ppf = print_operations ~empty:true ops ppf

let print_datatype (t, cnstrs) ppf =
  let print_clause (cnstr, topt) ppf =
    match topt with
    | None ->
       Format.fprintf ppf "@[<h>| %t@]" (Name.print cnstr)
    | Some t ->
       Format.fprintf ppf "@[<h>| %t of %t@]"
         (Name.print cnstr)
         (print_expr_ty ~max_level:Level.product t)
  in
  Format.fprintf ppf "@[<hov -2>%t =@\n@[<hv>%t@]@]"
                 (Name.print t)
                 (Print.sequence print_clause "" cnstrs)

let rec print_datatypes dfs ppf =
  match dfs with
  | [] -> ()
  | [df] -> print_datatype df ppf
  | df :: dfs ->
     Format.fprintf ppf "%t@\n and %t" (print_datatype df) (print_datatypes dfs)
