(** The desugared syntax. *)

(** The desugared syntax is an intermediate phase between the
    sugared syntax from {!module:Sugared} and the type-checked
    syntax {!module:Syntax}.
*)

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

(** Expression types *)
type expr_ty = expr_ty' Location.located
and expr_ty' =
  | Primitive of primitive
  | Abstract of Name.t
  | Alias of Name.t
  | Datatype of Name.t
  | Product of expr_ty list
  | ArrowUser of expr_ty * user_ty
  | ArrowKernel of expr_ty * kernel_ty
  | RunnerTy of operations * operations * signals * expr_ty
  | ContainerTy of operations

and user_ty =
  { user_ty : expr_ty
  ; user_ops : operations
  ; user_exc : exceptions }

and kernel_ty =
  { kernel_ty : expr_ty
  ; kernel_ops : operations
  ; kernel_exc : exceptions
  ; kernel_sgn : signals
  ; kernel_world : expr_ty }

(** The body of a datatype definition *)
type datatype = (Name.t * expr_ty option) list

(** Patterns *)
type pattern = pattern' Location.located
and pattern' =
  | PattAnonymous
  | PattVar of Name.t
  | PattNumeral of int
  | PattBoolean of bool
  | PattQuoted of string
  | PattConstructor of Name.t * pattern option
  | PattTuple of pattern list

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | ExprAscribe of expr * expr_ty
  | Var of Name.t
  | Numeral of int
  | Boolean of bool
  | Quoted of string
  | Tuple of expr list
  | Constructor of Name.t * expr option
  | FunUser of binder * user
  | FunKernel of binder * kernel
  | Runner of (Name.t * binder * kernel) list * expr_ty

(** User computations *)
and user = user' Location.located
and user' =
  | UserAscribe of user * user_ty
  | UserVal of expr
  | UserEqual of expr * expr
  | UserTry of user * user exception_handler
  | UserLet of pattern * user * user
  | UserLetRec of (Name.t * user_ty * pattern * expr_ty * user) list * user
  | UserApply of expr * expr
  | UserMatch of expr * (binder * user) list
  | UserOperation of Name.t * expr
  | UserRaise of Name.t * expr
  | UserUsing of expr * expr * user * finally
  | UserExec of kernel * expr * finally

(** Kernel computations *)
and kernel = kernel' Location.located
and kernel' =
  | KernelAscribe of kernel * kernel_ty
  | KernelVal of expr
  | KernelEqual of expr * expr
  | KernelTry of kernel * kernel exception_handler
  | KernelLet of pattern * kernel * kernel
  | KernelLetRec of (Name.t * kernel_ty * pattern * expr_ty * kernel) list * kernel
  | KernelApply of expr * expr
  | KernelMatch of expr * (binder * kernel) list
  | KernelOperation of Name.t * expr
  | KernelRaise of Name.t * expr
  | KernelKill of Name.t * expr
  | KernelGetenv
  | KernelSetenv of expr
  | KernelExec of user * kernel exception_handler

(** Exception handler *)
and 'a exception_handler = {
   exc_val : binder * 'a ;
   exc_raise : (Name.t * binder * 'a) list
}

and binder = pattern * expr_ty option

and finally =
  { fin_val : binder * binder * user
  ; fin_raise : (Name.t * binder * binder * user) list
  ; fin_kill : (Name.t * binder * user) list}

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * user
  | TopLetRec of (Name.t * user_ty * pattern * expr_ty * user) list
  | TopContainer of user
  | TopUser of user
  | DefineAbstract of Name.t
  | DefineAlias of Name.t * expr_ty
  | DefineDatatype of (Name.t * datatype) list
  | DeclareOperation of Name.t * expr_ty * expr_ty * exceptions
  | DeclareException of Name.t * expr_ty
  | DeclareSignal of Name.t * expr_ty
  | External of Name.t * expr_ty * string
