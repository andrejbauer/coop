(** Concrete syntax as parsed by the parser. *)

type signature = Name.t list

(** Parsed type. *)
type ty = ty' Location.located
and ty' =
  | Int
  | Product of ty list
  | Arrow of ty * ty
  | ComodelTy of signature * ty * signature
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
  | Tuple of term list
  | Match of term * (pattern * term) list
  | Lambda of abstraction * term
  | Apply of term * term
  | Let of pattern * term * term
  | Sequence of term * term
  | LetFun of Name.t * abstraction * term * term
  | Ascribe of term * ty
  | Comodel of term * comodel_clause list
  | Using of term * term * finally

and abstraction = (Name.t list * ty option) list

and comodel_clause = Name.t * pattern * pattern * term

and finally = pattern * pattern * term

(** Parsed top-level command. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of string
  | TopLet of pattern * term
  | TopLetFun of Name.t * abstraction * term
  | TopComp of term
  | DeclOperation of Name.t * ty * ty
  | External of Name.t * ty * string
