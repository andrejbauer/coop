(** Desugared syntax of Terminus. *)

(** Types. *)
type ty =
  | Int
  | Arrow of ty * ty
  | Product of ty list

(** Patterns *)
type pattern = pattern' Location.located
and pattern' =
  | PattAnonymous
  | PattVar of Name.ident
  | PattNumeral of int
  | PattTuple of pattern list

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | Var of Name.ident
  | Numeral of int
  | Tuple of expr list
  | Lambda of Name.ident * ty option * comp
  | AscribeExpr of expr * ty

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Return of expr
  | Sequence of Name.ident * comp * comp
  | Match of expr * (pattern * comp) list
  | Apply of expr * expr
  | AscribeComp of comp * ty

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of Name.ident * comp
  | TopComp of comp
  | DeclOperation of Name.ident * ty * ty
