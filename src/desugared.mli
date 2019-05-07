(** Desugared syntax of Coop. *)

type signature = Name.Set.t

(** Types. *)
type ty = ty' Location.located
and ty' =
  | Int
  | Arrow of ty * ty
  | Product of ty list
  | ComodelTy of signature * ty * signature
  | CompTy of ty * signature

(** Patterns *)
type pattern = pattern' Location.located
and pattern' =
  | PattAnonymous
  | PattVar of Name.t
  | PattNumeral of int
  | PattTuple of pattern list

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | AscribeExpr of expr * ty
  | Var of Name.t
  | Numeral of int
  | Tuple of expr list
  | Lambda of abstraction * comp
  | Comodel of expr * comodel_clause list

(** Computations *)
and comp = comp' Location.located
and comp' =
  | AscribeComp of comp * ty
  | Val of expr
  | Let of pattern * comp * comp
  | Match of expr * (pattern * comp) list
  | Apply of expr * expr
  | Operation of Name.t * expr
  | Using of expr * comp * finally

and abstraction = pattern * ty option

and comodel_clause = Name.t * pattern * pattern * comp

and finally = pattern * pattern * comp

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * comp
  | TopComp of comp
  | DeclOperation of Name.t * ty * ty
  | External of Name.t * ty * string
