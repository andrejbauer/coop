

type signature = {
    sig_ops : Name.Set.t ;
    sig_sgs : Name.Set.t
  }

(** Primitive types *)
type primitive =
  | Empty
  | Int
  | Bool
  | String

(** Types. *)
type ty = ty' Location.located
and ty' =
  | Primitive of primitive
  | Abstract of Name.t
  | Alias of Name.t
  | Datatype of Name.t
  | Arrow of ty * ty
  | Product of ty list
  | CohandlerTy of Name.Set.t * ty * signature
  | CompTy of ty * signature

(** The body of a datatype definition *)
type datatype = (Name.t * ty option) list

(** Patterns *)
type pattern = pattern' Location.located
and pattern' =
  | PattAnonymous
  | PattVar of Name.t
  | PattNumeral of int
  | PattBoolean of bool
  | PattQuoted of string
  | PattConstructor of Name.t * pattern option
  | PattTuple of pattern list

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | AscribeExpr of expr * ty
  | Var of Name.t
  | Numeral of int
  | Boolean of bool
  | Quoted of string
  | Tuple of expr list
  | Constructor of Name.t * expr option
  | Lambda of binder * comp
  | Cohandler of expr * cohandler_clause list
  | CohandlerRename of expr * (Name.t * Name.t) list
  | CohandlerTimes of expr * expr

(** Computations *)
and comp = comp' Location.located
and comp' =
  | AscribeComp of comp * ty
  | Val of expr
  | Let of pattern * comp * comp
  | LetRec of rec_clause list * comp
  | Match of expr * (binder * comp) list
  | Equal of expr * expr
  | Apply of expr * expr
  | Operation of Name.t * expr
  | Signal of Name.t * expr
  | Use of expr * comp * finally

and binder = pattern * ty option

and cohandler_clause = Name.t * binder * binder * comp

and rec_clause = Name.t * ty * pattern * ty * comp

and finally = {
    fin_val : binder * binder * comp ;
    fin_signals : (Name.t * binder * binder * comp) list
  }

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * comp
  | TopLetRec of rec_clause list
  | TopComp of comp
  | DefineAbstract of Name.t
  | DefineAlias of Name.t * ty
  | DefineDatatype of (Name.t * datatype) list
  | DeclareOperation of Name.t * ty * ty
  | DeclareSignal of Name.t * ty
  | External of Name.t * ty * string
