(** Concrete syntax as parsed by the parser. *)

(** Parsed type. *)
type ty =
  | Int
  | Product of ty list
  | Arrow of ty * ty

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
  | Match of term * (pattern * comp) list
  | Lambda of abstraction * comp
  | Apply of term * term
  | Let of pattern * comp * comp
  | LetFun of Name.t * abstraction * comp * comp
  | Ascribe of term * ty

and comp = term

and abstraction = (Name.t list * ty option) list

(** Parsed top-level command. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of string
  | TopLet of pattern * term
  | TopLetFun of Name.t * abstraction * comp
  | TopComp of comp
  | DeclOperation of Name.t * ty * ty
