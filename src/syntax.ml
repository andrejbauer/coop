(** Type-checked abstract syntax of Coop. *)

type operations = Operations of Name.Opset.t

type exceptions = Exceptions of Name.Idset.t

type signals = Signals of Name.Idset.t

(** Primitive types *)
type primitive =
  | Empty
  | Int
  | Bool
  | String

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
  | UserLetRec of rec_clause list * user
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
  | KernelLetRec of rec_clause list * kernel
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
  { try_val : (pattern * 'a) option
  ; try_raise : (Name.t * pattern * 'a) list }

and runner_clause = Name.t * pattern * kernel

and rec_clause =
  | RecUser of pattern * user
  | RecKernel of pattern * kernel

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * (Name.t * expr_ty) list * user
  | TopLetRec of rec_clause list * (Name.t * expr_ty) list
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

let empty_operations = Operations Name.Opset.empty
let empty_exceptions = Exceptions Name.Idset.empty
let empty_signals = Signals Name.Idset.empty

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
  ; user_ops = Operations (Name.Opset.union ops ops')
  ; user_exc = Exceptions (Name.Idset.union exc exc') }

(** Pollute a kernel type with given operations, exceptions, and signals *)
let pollute_kernel {kernel_ty; kernel_ops=Operations ops; kernel_exc=Exceptions exc; kernel_sgn=Signals sgn; kernel_world}
                 (Operations ops') (Exceptions exc') (Signals sgn') =
  { kernel_ty
  ; kernel_ops = Operations (Name.Opset.union ops ops')
  ; kernel_exc = Exceptions (Name.Idset.union exc exc')
  ; kernel_sgn = Signals (Name.Idset.union sgn sgn')
  ; kernel_world }

(** The user type of the given operation [op] *)
let operation_user_ty t op exc =
  { user_ty = t
  ; user_ops = Operations (Name.Opset.add op Name.Opset.empty)
  ; user_exc = exc }

(** The kernel type of the given operation [op] *)
let operation_kernel_ty t op exc tw =
  { kernel_ty = t
  ; kernel_ops = Operations (Name.Opset.add op Name.Opset.empty)
  ; kernel_exc = exc
  ; kernel_sgn = empty_signals
  ; kernel_world = tw }

(** The user type of a raise *)
let raise_user_ty exc =
  { user_ty = Primitive Empty
  ; user_ops = empty_operations
  ; user_exc = Exceptions (Name.Idset.add exc Name.Idset.empty) }

(** The kernel type of a raise *)
let raise_kernel_ty exc w_ty =
  { kernel_ty = Primitive Empty
  ; kernel_ops = empty_operations
  ; kernel_exc = Exceptions (Name.Idset.add exc Name.Idset.empty)
  ; kernel_sgn = empty_signals
  ; kernel_world = w_ty }

(** The kernel type of a kill *)
let kill_ty sgn w_ty =
  { kernel_ty = Primitive Empty
  ; kernel_ops = empty_operations
  ; kernel_exc = empty_exceptions
  ; kernel_sgn = Signals (Name.Idset.add sgn Name.Idset.empty)
  ; kernel_world = w_ty }

(** Pretty-print a primitive type *)
let print_primitive p ppf =
  Format.fprintf ppf
  (match p with
  | Empty -> "empty"
  | Int -> "int"
  | Bool -> "bool"
  | String -> "string")

(** Pretty-print effect information *)
type effect =
  | Operation of Name.t
  | Exception of Name.t
  | Signal of Name.t

let effects ~ops ~exc ~sgn =
  List.map (fun o -> Operation o) (Name.Opset.elements ops) @
  List.map (fun e -> Exception e) (Name.Idset.elements exc) @
  List.map (fun s -> Signal s) (Name.Idset.elements sgn)

let print_effect eff ppf =
  match eff with
  | Operation o -> Name.print ~parentheses:true o ppf
  | Exception e -> Print.exception_name e ppf
  | Signal s -> Print.signal_name s ppf

let print_effects ~ops ~exc ~sgn ppf =
  Format.fprintf ppf "{%t}" (Print.sequence print_effect "," (effects ~ops ~exc ~sgn))

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

  | ArrowUser (t1, t2) ->
     Print.print ?max_level ~at_level:Level.arr ppf "%t@ %s@ %t"
       (print_expr_ty ~max_level:Level.arr_left t1)
       (Print.char_arrow ())
       (print_user_ty ~max_level:Level.arr_right t2)

  | ArrowKernel (t1, t2) ->
     Print.print ?max_level ~at_level:Level.arr ppf "%t@ %s@ %t"
       (print_expr_ty ~max_level:Level.arr_left t1)
       (Print.char_arrow ())
       (print_kernel_ty ~max_level:Level.arr_right t2)

  | RunnerTy rnr_ty -> print_runner_ty rnr_ty ppf

  | ContainerTy (Operations ops) ->
     Format.fprintf ppf "{%t}"
       (Print.names ops)

and print_user_ty ?max_level {user_ty=t; user_ops=Operations ops; user_exc=Exceptions exc} ppf =
    Print.print ?max_level ~at_level:Level.user_ty ppf "%t@ %t"
      (print_expr_ty ~max_level:Level.user_ty_left t)
      (print_effects ~ops ~exc ~sgn:Name.Set.empty)

and print_kernel_ty ?max_level {kernel_ty=t;
                                kernel_ops=Operations ops;
                                kernel_exc=Exceptions exc;
                                kernel_sgn=Signals sgn;
                                kernel_world=wt} ppf =
    Print.print ?max_level ~at_level:Level.kernel_ty ppf "%t@ %t @@@ %t"
      (print_expr_ty ~max_level:Level.kernel_ty_left t)
      (print_effects ~ops ~exc ~sgn)
      (print_expr_ty ~max_level:Level.world_ty wt)

and print_runner_ty (Operations ops1, Operations ops2, Signals sgns, wt) ppf =
  Format.fprintf ppf "%t@ %s@ %t@ @@@ %t"
    (print_effects ~ops:ops1 ~exc:Name.Set.empty ~sgn:Name.Set.empty)
    (Print.char_darrow ())
    (print_effects ~ops:ops2 ~exc:Name.Set.empty ~sgn:sgns)
    (print_expr_ty ~max_level:Level.runner_ty_world wt)

and print_container_ty (Operations ops) ppf =
  print_effects ~ops ~exc:Name.Set.empty ~sgn:Name.Set.empty ppf

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
