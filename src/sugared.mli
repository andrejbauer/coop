(** Concrete syntax as parsed by the parser. *)

type signature = Name.t list

(** Primitive types *)
type primitive =
  | Empty
  | Int
  | Bool
  | String

(** Parsed type. *)
type ty = ty' Location.located
and ty' =
  | Primitive of primitive
  | NamedTy of Name.t
  | Product of ty list
  | Arrow of ty * ty
  | CohandlerTy of Name.t list * ty * signature
  | ShellTy of Name.t list
  | CompTy of ty * signature

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
  | Cohandler of term * cohandler_clause list
  | CohandlerTimes of term * term
  | CohandlerRename of term * (Name.t * Name.t) list
  | Use of term * term * finally_clause list

and binder = pattern * ty option

and typed_binder = pattern * ty

and cohandler_clause = Name.t * binder * binder * term

and rec_clause = Name.t * typed_binder * typed_binder list * ty * term

and let_binding =
  | BindVal of pattern * ty option * term
  | BindFun of Name.t * binder list * ty option * term

and finally_clause =
  | FinVal of binder * binder * term
  | FinSignal of Name.t * binder * binder * term

(** Parsed top-level command. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of string
  | TopLet of let_binding
  | TopLetRec of rec_clause list
  | TopShell of term
  | TopComp of term
  | DefineAbstract of Name.t
  | DefineAlias of Name.t * ty
  | DefineDatatype of (Name.t * datatype) list
  | DeclareOperation of Name.t * ty * ty
  | DeclareSignal of Name.t * ty
  | External of Name.t * ty * string
