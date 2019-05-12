(** Concrete syntax as parsed by the parser. *)

type signature = Name.t list

(** Parsed type. *)
type ty = ty' Location.located
and ty' =
  | Int
  | Bool
  | Product of ty list
  | Arrow of ty * ty
  | ComodelTy of Name.t list * ty * signature
  | CompTy of ty * signature

(** Pattern *)
type pattern = pattern' Location.located
and pattern' =
  | PattAnonymous
  | PattVar of Name.t
  | PattNumeral of int
  | PattTuple of pattern list

(** Parsed expressions/computations *)
type term = term' Location.located
and term' =
  | Var of Name.t
  | Numeral of int
  | False
  | True
  | Tuple of term list
  | Match of term * (binder * term) list
  | If of term * term * term
  | Lambda of binder list * term
  | Apply of term * term
  | Let of pattern * term * term
  | Sequence of term * term
  | LetFun of Name.t * binder list * term * term
  | Ascribe of term * ty
  | Comodel of ty * comodel_clause list
  | Using of term * term * term * finally_clause list

and binder = pattern * ty option

and comodel_clause = Name.t * binder * binder * term

and finally_clause =
  | FinVal of binder * binder * term
  | FinSignal of Name.t * binder * binder * term

(** Parsed top-level command. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of string
  | TopLet of pattern * term
  | TopLetFun of Name.t * binder list * term
  | TopComp of term
  | DeclOperation of Name.t * ty * ty
  | DeclSignal of Name.t * ty
  | External of Name.t * ty * string
