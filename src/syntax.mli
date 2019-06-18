(** Type-checked syntax of Coop. *)

(** A signature is a set of operation names and a set of signal names. We need
   not carry the types of the operations and signals because those are declared globally. *)

type operations = Name.Set.t

type signals = Name.Set.t

type signature = {
    sig_ops : operations ;
    sig_sgs : signals
  }

(** Primitive types *)
type primitive =
  | Empty
  | Int
  | Bool
  | String
  | Any

(** Expression type *)
type expr_ty =
  | Abstract of Name.t
  | Alias of Name.t
  | Datatype of Name.t
  | Primitive of primitive
  | Product of expr_ty list
  | Arrow of expr_ty * comp_ty
  | RunnerTy of runner_ty
  | ShellTy of operations

(** Computation type *)
and comp_ty = { comp_ty : expr_ty ; comp_sig : signature }

(** Runner *)
and runner_ty = operations * expr_ty * signature

(** The body of a datatype definition *)
type datatype = (Name.t * expr_ty option) list

(** Patterns *)
type pattern =
  | PattAnonymous
  | PattVar
  | PattNumeral of int
  | PattBoolean of bool
  | PattQuoted of string
  | PattConstructor of Name.t * pattern option
  | PattTuple of pattern list

(** De Bruijn index *)
type index = int

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | Var of index
  | Numeral of int
  | Boolean of bool
  | Quoted of string
  | Constructor of Name.t * expr option
  | Tuple of expr list
  | Lambda of pattern * comp
  | Runner of (Name.t * pattern * pattern * comp) list
  | RunnerTimes of expr * expr
  | RunnerRename of expr * Name.t Name.Map.t

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Val of expr
  | Let of pattern * comp * comp
  | LetRec of (pattern * comp) list * comp
  | Match of expr * (pattern * comp) list
  | Equal of expr * expr
  | Apply of expr * expr
  | Operation of Name.t * expr
  | Signal of Name.t * expr
  | Run of expr * expr * comp * finally
  | Try of comp * trying

and finally = {
    fin_val : pattern * pattern * comp ;
    fin_signals : (Name.t * pattern * pattern * comp) list
}

and trying = {
    try_val : pattern * comp ;
    try_signals : (Name.t * pattern * comp) list
}


(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * (Name.t * expr_ty) list * comp
  | TopLetRec of (pattern * comp) list * (Name.t * expr_ty) list
  | TopShell of comp * operations
  | TopComp of comp * expr_ty
  | DefineAbstract of Name.t
  | DefineAlias of Name.t * expr_ty
  | DefineDatatype of (Name.t * datatype) list
  | DeclareOperation of Name.t * expr_ty * expr_ty
  | DeclareSignal of Name.t * expr_ty
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

(** Print an expression type *)
val print_expr_ty : ?max_level:Level.t -> expr_ty -> Format.formatter -> unit

(** Print a computation type *)
val print_comp_ty : ?max_level:Level.t -> comp_ty -> Format.formatter -> unit

(** Print a shell type *)
val print_shell_ty : operations -> Format.formatter -> unit

(** Print the body of a datatype definition *)
val print_datatype : Name.t * datatype -> Format.formatter -> unit

(** Print the bodies of datatype definitions *)
val print_datatypes : (Name.t * datatype) list -> Format.formatter -> unit
