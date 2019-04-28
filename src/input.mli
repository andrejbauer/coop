(** Concrete syntax as parsed by the parser. *)

(** Parsed type. *)
type ty =
  | Int
  | Arrow of ty * ty

(** Parsed expressions/computations *)
type expr = expr' Location.located
and expr' =
  | Var of Name.ident
  | Numeral of int
  | Lambda of (Name.ident list * ty option) list * comp
  | Apply of expr * expr
  | Let of Name.ident * comp * comp

and comp = expr

(** Parsed top-level command. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of string
  | TopLet of Name.ident * expr
  | TopComp of comp
