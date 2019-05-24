(** Concrete syntax as parsed by the parser. *)

type signature = Name.t list

(** Primitive types *)
type primitive =
  | Empty
  | Int
  | Bool
  | StringTy

(** Parsed type. *)
type ty = ty' Location.located
and ty' =
  | Primitive of primitive
  | NamedTy of Name.t
  | Product of ty list
  | Arrow of ty * ty
  | ComodelTy of Name.t list * ty * signature
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
  | PattString of string
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
  | String of string
  | Tuple of term list
  | Match of term * (binder * term) list
  | If of term * term * term
  | Lambda of binder list * term
  | Apply of term * term
  | Let of pattern * term * term
  | LetRec of rec_clause list * term
  | Sequence of term * term
  | LetFun of Name.t * binder list * term * term
  | Ascribe of term * ty
  | Comodel of term * comodel_clause list
  | ComodelTimes of term * term
  | ComodelRename of term * (Name.t * Name.t) list
  | Using of term * term * finally_clause list

and binder = pattern * ty option

and typed_binder = pattern * ty

and comodel_clause = Name.t * binder * binder * term

and rec_clause = Name.t * typed_binder * typed_binder list * ty * term

and finally_clause =
  | FinVal of binder * binder * term
  | FinSignal of Name.t * binder * binder * term

(** Parsed top-level command. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of string
  | TopLet of pattern * term
  | TopLetFun of Name.t * binder list * term
  | TopLetRec of rec_clause list
  | TopComp of term
  | DefineAbstract of Name.t
  | DefineAlias of Name.t * ty
  | DefineDatatype of (Name.t * datatype) list
  | DeclareOperation of Name.t * ty * ty
  | DeclareSignal of Name.t * ty
  | External of Name.t * ty * string
