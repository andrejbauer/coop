(** The desugared syntax. *)

(** The desugared syntax is an intermediate phase between the
    sugared syntax from {!module:Sugared} and the type-checked
    syntax {!module:Syntax}.
*)

type operations = Operations of Name.Opset.t

type exceptions = Exceptions of Name.Idset.t

type signals = Signals of Name.Idset.t

(** Primitive types *)
type primitive =
  | Empty
  | Int
  | Bool
  | String

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

and user_ty = user_ty' Location.located
and user_ty' =
  { user_ty : expr_ty
  ; user_ops : operations
  ; user_exc : exceptions }

and kernel_ty = kernel_ty' Location.located
and kernel_ty' =
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
  | AscribeExpr of expr * expr_ty
  | Var of Name.t
  | Numeral of int
  | Boolean of bool
  | Quoted of string
  | Tuple of expr list
  | Constructor of Name.t * expr option
  | FunUser of binder * comp
  | FunKernel of binder * expr_ty * comp
  | Runner of (Name.t * binder * comp) list * expr_ty

(** Computations *)
and comp = comp' Location.located
and comp' =
  | AscribeUser of comp * user_ty
  | AscribeKernel of comp * kernel_ty
  | Val of expr
  | Equal of expr * expr
  | Try of comp * exception_handler
  | Let of pattern * comp * comp
  | LetRec of rec_clause list * comp
  | Apply of expr * expr
  | Match of expr * (binder * comp) list
  | Operation of Name.t * expr
  | Raise of Name.t * expr
  | Kill of Name.t * expr
  | Getenv
  | Setenv of expr
  | Using of expr * expr * comp * finally
  | ExecKernel of comp * expr * finally
  | ExecUser of comp * exception_handler

(** Exception handler *)
and exception_handler = {
   try_val : (binder * comp) option ;
   try_raise : (Name.t * binder * comp) list
}

and binder = pattern * expr_ty option

and finally =
  { fin_val : binder * binder * comp
  ; fin_raise : (Name.t * binder * binder * comp) list
  ; fin_kill : (Name.t * binder * comp) list}

and rec_clause =
  | RecUser of Name.t * pattern * expr_ty * user_ty * comp
  | RecKernel of Name.t * pattern * expr_ty * kernel_ty * comp

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * comp
  | TopLetRec of rec_clause list
  | TopContainer of comp
  | TopUser of comp
  | DefineAbstract of Name.t
  | DefineAlias of Name.t * expr_ty
  | DefineDatatype of (Name.t * datatype) list
  | DeclareOperation of Name.t * expr_ty * expr_ty * exceptions
  | DeclareException of Name.t * expr_ty
  | DeclareSignal of Name.t * expr_ty
  | External of Name.t * expr_ty * string
