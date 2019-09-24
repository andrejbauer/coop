(** Concrete syntax as parsed by the parser. *)

(** Operations, exceptions and signals *)
type effect =
  | Operation of Name.t
  | Exception of Name.t
  | Signal of Name.t

type signature = effect list

(** Primitive types *)
type primitive =
  | Empty
  | Int
  | Bool
  | String
  | Any

(** Parsed type. *)
type ty = ty' Location.located
and ty' =
  | Primitive of primitive
  | NamedTy of Name.t
  | Product of ty list
  | Arrow of ty * ty
  | RunnerTy of signature * signature * ty
  | ContainerTy of signature
  | ComputationTy of ty * signature * ty option

(** The body of a datatype definition *)
type datatype = (Name.t * ty option) list

(** Pattern *)
type pattern = pattern' Location.located
and pattern' =
  | PattAnonymous
  | PattVar of Name.t
  | PattNumeral of int
  | PattBoolean of bool
  | PattQuoted of string
  | PattConstructor of Name.t * pattern option
  | PattTuple of pattern list

(** Parsed expressions/computations *)
type term = term' Location.located
and term' =
  | Var of Name.t
  | Numeral of int
  | False
  | True
  | Constructor of Name.t
  | Quoted of string
  | Tuple of term list
  | Equal of term * term
  | Match of term * (binder * term) list
  | If of term * term * term
  | Lambda of binder list * term
  | Apply of term * term
  | Let of let_binding * term
  | LetRec of rec_clause list * term
  | Sequence of term * term
  | Ascribe of term * ty
  | Runner of runner_clause list * ty
  | Run of term * term * term * finally_clause list
  | Try of term * try_clause list
  | Getenv
  | Setenv of term
  | Raise of term
  | Kill of term

and binder = pattern * ty option

and typed_binder = pattern * ty

and runner_clause = Name.t * binder * term

and rec_clause = Name.t * typed_binder * typed_binder list * ty * term

and let_binding =
  | BindVal of pattern * ty option * term
  | BindFun of Name.t * binder list * ty option * term

and finally_clause =
  | FinVal of binder * binder * term
  | FinRaise of Name.t * binder * binder * term
  | FinKill of Name.t * binder * term

and try_clause =
  | TryVal of binder * term
  | TryKill of Name.t * binder * term
  | TryRaise of Name.t * binder * term

(** Parsed top-level command. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of string
  | TopLet of let_binding
  | TopLetRec of rec_clause list
  | TopContainer of term
  | TopComp of term
  | DefineAbstract of Name.t
  | DefineAlias of Name.t * ty
  | DefineDatatype of (Name.t * datatype) list
  | DeclareOperation of Name.t * ty * ty
  | DeclareException of Name.t * ty
  | DeclareSignal of Name.t * ty
  | External of Name.t * ty * string
