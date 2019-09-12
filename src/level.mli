(** Precedence of prefix and infix operators. *)

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

(** The level of a prefix operator *)
val prefix : t

(** The level of the argument of a prefix operator *)
val prefix_arg : t

(** The level of an infix operator, and its left and right arguments *)
val infix : infix -> t * t * t

(** The level of a datatype constructor *)
val constr : t

(** The level of the argument of a constructor *)
val constr_arg : t

(** The level of a product type *)
val product : t

(** The level of the argument of a product type *)
val product_arg : t

(** The level of a tuple *)
val tuple : t

(** The level of the arguments of a tuple *)
val tuple_arg : t

(** The level of an arrow type *)
val arr : t

(** The level of the left argument of an arrow type *)
val arr_left : t

(** The level of the right argument of an arrow type *)
val arr_right : t

(** The level of a runner type *)
val runner_ty : t

(** The level of the runner type world argument *)
val runner_ty_world : t

(** The level of a user type with its dirt *)
val user_ty : t
val user_ty_left : t

(** The level of a kernel type with its dirt *)
val kernel_ty : t
val kernel_ty_left : t

(** The level of a world type *)
val world_ty : t
