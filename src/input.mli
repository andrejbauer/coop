(** Concrete syntax as parsed by the parser. *)

(** Parsed type. *)
type ty =
  | Int
  | Arrow of ty * ty

(** Parsed expressions/computations *)
type term = term' Location.located
and term' =
  | Var of Name.ident
  | Numeral of int
  | Lambda of (Name.ident list * ty option) list * comp
  | Apply of term * term
  | Let of Name.ident * comp * comp
  | Ascribe of term * ty

and comp = term

(** Parsed top-level command. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of string
  | TopLet of Name.ident * term
  | TopComp of comp
  | DeclOperation of Name.ident * ty * ty
