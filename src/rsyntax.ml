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

(** Convert an expression type to a computation type. *)
let purely ty = CompTy (ty, ())

(** Compute the expression-type part of a computation type *)
let purify (CompTy (ty, ())) = ty

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

  | Arrow (t1, t2) ->
     Print.print ?max_level ~at_level:Level.arr ppf "@[<hov>%t@ %s@ %t@]"
       (print_expr_ty ~max_level:Level.arr_left t1)
       (Print.char_arrow ())
       (print_comp_ty ~max_level:Level.arr_right t2)

(** Pretty-print a computation type *)
and print_comp_ty ?max_level ty ppf =
  match ty with

  | CompTy (ty, ()) ->
     print_expr_ty ?max_level ty ppf
