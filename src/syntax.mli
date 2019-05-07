(** Type-checked syntax of Coop. *)

(** A signature is a set of operation names. We need not carry the types of the
   operations because those are declared globally. *)
type signature = Name.Set.t

(** Expression type *)
type expr_ty =
  | Int
  | Product of expr_ty list
  | Arrow of expr_ty * comp_ty
  | ComodelTy of comodel_ty

(** Computation type *)
and comp_ty = CompTy of expr_ty * signature

(** Comodel *)
and comodel_ty = signature * expr_ty * signature

(** Patterns *)
type pattern =
  | PattAnonymous
  | PattVar
  | PattNumeral of int
  | PattTuple of pattern list

(** De Bruijn index *)
type index = int

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | Var of index
  | Numeral of int
  | Tuple of expr list
  | Lambda of comp
  | Comodel of expr * (Name.t * pattern * pattern * comp) list

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Val of expr
  | Let of pattern * comp * comp
  | Match of expr * (pattern * comp) list
  | Apply of expr * expr
  | Operation of Name.t * expr
  | Using of expr * comp * finally

and finally = pattern * pattern * comp

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * (Name.t * expr_ty) list * comp
  | TopComp of comp * expr_ty
  | DeclOperation of Name.t * expr_ty * expr_ty
  | External of Name.t * expr_ty * string

(** The unit type *)
val unit_ty : expr_ty

(** The empty signature *)
val empty_signature : signature

(** Make a pure computation type *)
val pure : expr_ty -> comp_ty

(** Add more dirt to a computation type *)
val pollute : comp_ty -> signature -> comp_ty

(** Add an operation to a computation type *)
val op_ty : expr_ty -> Name.t -> comp_ty

(** Is the first expression type a subtype of the second one? *)
val expr_subty : expr_ty -> expr_ty -> bool

(** Is the first computation type a subtype of the second one? *)
val comp_subty : comp_ty -> comp_ty -> bool

(** Print an expression type *)
val print_expr_ty : ?max_level:Level.t -> expr_ty -> Format.formatter -> unit

(** Print a computation type *)
val print_comp_ty : ?max_level:Level.t -> comp_ty -> Format.formatter -> unit
