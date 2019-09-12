(** Type-checked syntax of Coop. *)

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
  | UserOperation of Name.t * expr
  | UserRaise of Name.t * expr
  | UserUsing of expr * expr * user * finally
  | UserRun of kernel * expr * finally

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
  | KernelOperation of Name.t * expr
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
  | DeclareSignal of Name.t * expr_ty
  | External of Name.t * expr_ty * string

(** The unit type *)
val unit_ty : expr_ty

(** The empty entities *)
val empty_operations : operations

(** Make a pure user type *)
val pure_user_ty : expr_ty -> user_ty

(** Make a pure kernel type *)
val pure_kernel_ty : expr_ty -> expr_ty -> kernel_ty

(** Make a user computation type with a single operation. *)
val operation_user_ty : expr_ty -> Name.t -> user_ty

(** Make a kernel computation type with a single operation. *)
val operation_kernel_ty : expr_ty -> Name.t -> expr_ty -> kernel_ty

(** Print an expression type *)
val print_expr_ty : ?max_level:Level.t -> expr_ty -> Format.formatter -> unit

(** Print a user computation type *)
val print_user_ty : ?max_level:Level.t -> user_ty -> Format.formatter -> unit

(** Print a kernel computation type *)
val print_kernel_ty : ?max_level:Level.t -> kernel_ty -> Format.formatter -> unit

(** Print a shell type *)
val print_container_ty : operations -> Format.formatter -> unit

(** Print the body of a datatype definition *)
val print_datatype : Name.t * datatype -> Format.formatter -> unit

(** Print the bodies of datatype definitions *)
val print_datatypes : (Name.t * datatype) list -> Format.formatter -> unit
