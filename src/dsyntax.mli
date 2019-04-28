(** Desugared syntax of Terminus. *)

(** Types. *)
type ty =
  | Int
  | Arrow of ty * ty

(** De Bruijn index *)
type index = int

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | Var of index
  | Numeral of int
  | Lambda of Name.ident * ty option * comp
  | AscribeExpr of expr * ty

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Return of expr
  | Sequence of Name.ident * comp * comp
  | Apply of expr * expr
  | AscribeComp of comp * ty

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of Name.ident * comp
  | TopComp of comp
  | DeclOperation of Name.ident * ty * ty
