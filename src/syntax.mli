(** Type-checked syntax of Coop. *)

(** A signature is a set of operation names and a set of signal names. We need
   not carry the types of the operations and signals because those are declared globally. *)
type signature = {
    sig_ops : Name.Set.t ;
    sig_sgs : Name.Set.t
  }

(** Expression type *)
type expr_ty =
  | SignalTy
  | Int
  | Bool
  | Product of expr_ty list
  | Arrow of expr_ty * comp_ty
  | ComodelTy of comodel_ty

(** Computation type *)
and comp_ty = CompTy of expr_ty * signature

(** Comodel *)
and comodel_ty = Name.Set.t * expr_ty * signature

(** Patterns *)
type pattern =
  | PattAnonymous
  | PattVar
  | PattNumeral of int
  | PattBoolean of bool
  | PattTuple of pattern list

(** De Bruijn index *)
type index = int

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | Var of index
  | Numeral of int
  | Boolean of bool
  | Tuple of expr list
  | Lambda of pattern * comp
  | Comodel of (Name.t * pattern * pattern * comp) list

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Val of expr
  | Let of pattern * comp * comp
  | Match of expr * (pattern * comp) list
  | Apply of expr * expr
  | Operation of Name.t * expr
  | Signal of Name.t * expr
  | Using of expr * expr * comp * finally

and finally = {
    fin_val : pattern * pattern * comp ;
    fin_signals : (Name.t * pattern * pattern * comp) list
}

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * (Name.t * expr_ty) list * comp
  | TopComp of comp * expr_ty
  | DeclOperation of Name.t * expr_ty * expr_ty
  | DeclSignal of Name.t * expr_ty
  | External of Name.t * expr_ty * string

(** The unit type *)
val unit_ty : expr_ty

(** The empty signature *)
val empty_signature : signature

(** Make a pure computation type *)
val pure : expr_ty -> comp_ty

(** Add more dirt to a computation type *)
val pollute : comp_ty -> signature -> comp_ty

(** Make a computation type with a single operation. *)
val operation_ty : expr_ty -> Name.t -> comp_ty

(** Make a computation type with a single signal. *)
val signal_ty : Name.t -> comp_ty

(** Is the first expression type a subtype of the second one? *)
val expr_subty : expr_ty -> expr_ty -> bool

(** Is the first computation type a subtype of the second one? *)
val comp_subty : comp_ty -> comp_ty -> bool

(** Is the first signature a subsignature of the second one? *)
val subsignature : signature -> signature -> bool

(** Join signatures by taking the union of operations and signals *)
val join_signature : signature -> signature -> signature

(** Meet signatures by taking the intersection of operations and signals *)
val meet_signature : signature -> signature -> signature

(** Print an expression type *)
val print_expr_ty : ?max_level:Level.t -> expr_ty -> Format.formatter -> unit

(** Print a computation type *)
val print_comp_ty : ?max_level:Level.t -> comp_ty -> Format.formatter -> unit
