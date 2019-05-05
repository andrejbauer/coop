(** Type-checked syntax of Terminus. *)

(** For now dirt is trivial *)
type dirt = unit

(** Expression type *)
type expr_ty =
  | Int
  | Product of expr_ty list
  | Arrow of expr_ty * comp_ty
  | ComodelTy of (Name.t * expr_ty * comp_ty) list

(** Computation type *)
and comp_ty = CompTy of expr_ty * dirt

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
  | Comodel of (Name.t * comp) list

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Return of expr
  | Let of pattern * comp * comp
  | Match of expr * (pattern * comp) list
  | Apply of expr * expr

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * (Name.t * expr_ty) list * comp
  | TopComp of comp * comp_ty
  | DeclOperation of Name.t * expr_ty * comp_ty

(** Convert an expression type to a computation type. *)
let purely ty = CompTy (ty, ())

(** Compute the expression-type part of a computation type *)
let purify (CompTy (ty, ())) = ty

(** Add an operation to the dirt *)
let pollute ty op = ty

let equal_dirt d1 d2 = (d1 = d2)

(** Compare two expression types for equality. *)
let equal_expr_ty t1 t2 = (t1 = t2)

(** Compare two computation types for equality. *)
let equal_comp_ty (CompTy (t1, d1)) (CompTy (t2, d2)) =
  equal_expr_ty t1 t2 && equal_dirt d1 d2

(** Pretty-print an expresion type *)
let rec print_expr_ty ?max_level ty ppf =
  match ty with

  | Int -> Format.fprintf ppf "int"

  | Product lst ->
     let st = " " ^ Print.char_times () in
     Print.print ?max_level ~at_level:Level.product ppf "%t"
       (Print.sequence (print_expr_ty ~max_level:Level.product_arg) st lst)

  | Arrow (t1, t2) ->
     Print.print ?max_level ~at_level:Level.arr ppf "%t@ %s@ %t"
       (print_expr_ty ~max_level:Level.arr_left t1)
       (Print.char_arrow ())
       (print_comp_ty ~max_level:Level.arr_right t2)

  | ComodelTy lst ->
     Print.print ?max_level ~at_level:Level.no_parens ppf "{%t}"
        (Print.sequence (print_op_type ~max_level:Level.no_parens) ";" lst)

and print_op_type ?max_level (op, ty1, ty2) ppf =
  Print.print ?max_level ~at_level:Level.no_parens ppf "%t@ :@ %t@ %s@ %t"
    (Name.print op)
    (print_expr_ty ~max_level:Level.arr_left ty1)
    (Print.char_arrow ())
    (print_comp_ty ~max_level:Level.arr_right ty2)

(** Pretty-print a computation type *)
and print_comp_ty ?max_level ty ppf =
  match ty with

  | CompTy (ty, ()) ->
     print_expr_ty ?max_level ty ppf
