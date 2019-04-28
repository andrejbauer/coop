(** Runtime value *)
type value =
  | Numeral of int
  | Closure of closure

(** Runtime result *)
and result =
  | Return of value
  | Operation of Name.ident * value * closure

and closure = value -> result

(** Runtime environment *)
type environment

(** Runtime error *)
type error

(** Runtime exception for raising an error *)
exception Error of error Location.located

(** Print a runtime error *)
val print_error : error -> Format.formatter -> unit

(** The initial runtime environment *)
val initial : environment

(** Extend the runtime environment with the given value *)
val extend : value -> environment -> environment

(** Lookup a DeBruijn index in the runtime environment *)
val lookup : loc:Location.t -> int -> environment -> value

(** Create a generic operation *)
val generic : Name.ident -> value

(** Print a runtime value *)
val print_value : ?max_level:Level.t -> value -> Format.formatter -> unit

(** Convert a result to a value, or raise an error if the result is an operation *)
val as_value : loc:Location.t -> result -> value

(** Convert an expression to a closure, if possible *)
val as_closure : loc:Location.t -> value -> closure
