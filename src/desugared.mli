(** Desugared syntax of Terminus. *)

(** Types. *)
type ty =
  | Int
  | Arrow of ty * ty
  | Product of ty list
  | ComodelTy of (Name.t * ty * ty) list

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
  | Var of Name.t
  | Numeral of int
  | Tuple of expr list
  | Lambda of abstraction * comp
  | Comodel of comodel_clause list
  | AscribeExpr of expr * ty

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Return of expr
  | Let of pattern * comp * comp
  | Match of expr * (pattern * comp) list
  | Apply of expr * expr
  | AscribeComp of comp * ty

and abstraction = Name.t * ty option

and comodel_clause = Name.t * abstraction * comp

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * comp
  | TopComp of comp
  | DeclOperation of Name.t * ty * ty
