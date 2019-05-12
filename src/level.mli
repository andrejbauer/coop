(** Precedence of operators *)

(** Levels of precedence -- higher level is less likely to be parenthesized. *)
type t

(** If we print [at_level] where [max_level] is the highest level that can still
    be printed without parenthesis, should we print parenthesis? *)
val parenthesize : at_level:'a -> max_level:'a -> bool

(** Following OCaml syntax, there are five levels of infix operators *)
type infix = Infix0 | Infix1 | Infix2 | Infix3 | Infix4 | Infix5

(** The highest possible level *)
val highest : t

(** The least possible level *)
val least : t

(** The level which never gets parenthesized (equal to [least]) *)
val no_parens : t

(** The level of a prefix operator and its argument *)
val prefix : t
val prefix_arg : t

(** The level of an infix operator, and its left and right arguments *)
val infix : infix -> t * t * t

(** The level of a constructor application *)
val constr : t
val constr_arg : t

(** The level of a product type and its arguments *)
val product : t
val product_arg : t

val tuple : t
val tuple_arg : t

(** The level of an arrow and its arguments *)
val arr : t
val arr_left : t
val arr_right : t

(** The level of a comodel type and its world argument *)
val comodel_ty : t
val comodel_ty_world : t

(** The level of a computation type with its dirt *)
val comp_ty : t
val comp_ty_left : t
val comp_ty_right : t
