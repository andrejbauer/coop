(** Runtime values *)

(** Runtime value *)
type t =
  | Abstract
  | Numeral of int
  | Boolean of bool
  | Quoted of string
  | Constructor of Name.t * t option
  | Tuple of t list
  | ClosureUser of (t -> t user_result)
  | ClosureKernel of (t -> t kernel_result)
  | Runner of cooperation Name.Map.t
  | Container of container

and world = World of t

and 'a user_result =
  | UserVal of 'a
  | UserOperation of Name.t * t * (t -> 'a user_result)
  | UserException of Name.t * t

and 'a kernel_result =
  | KernelVal of 'a
  | KernelOperation of Name.t * t * (t -> 'a kernel_result)
  | KernelException of Name.t * t
  | KernelSignal of Name.t * t

and cooperation = t * world -> (t * world) kernel_result

and container = (t * world -> t * world) Name.Map.t * world

(** Give a descriptive name of a value. *)
val name : t -> string

(** Give a descriptive name of a value, in plural. *)
val names : t -> string

(** The empty container *)
val pure_container : container

(** Pretty-print a value. *)
val print : ?max_level:Level.t -> t -> Format.formatter -> unit
