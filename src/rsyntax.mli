(** Type-checked syntax of Terminus. *)

(** For now dirt is trivial *)
type dirt = unit

(** Expression type *)
type expr_ty =
  | Int
  | Arrow of expr_ty * comp_ty

(** Computation type *)
and comp_ty = CompTy of expr_ty * dirt

(** De Bruijn index *)
type index = int

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | Var of index
  | Numeral of int
  | Lambda of comp

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Return of expr
  | Sequence of comp * comp
  | Apply of expr * expr

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of Name.ident * expr_ty * comp
  | TopComp of comp * comp_ty

(** Make a pure computation type *)
val purely : expr_ty -> comp_ty

(** The expression type associated with the given computation type *)
val purify : comp_ty -> expr_ty

(** Are expression types equal? *)
val equal_expr_ty : expr_ty -> expr_ty -> bool

(** Are computation types equal? *)
val equal_comp_ty : comp_ty -> comp_ty -> bool

(** Print an expression type *)
val print_expr_ty : ?max_level:Level.t -> expr_ty -> Format.formatter -> unit

(** Print a computation type *)
val print_comp_ty : ?max_level:Level.t -> comp_ty -> Format.formatter -> unit