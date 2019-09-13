(** Runtime values *)

(** The carrier of the user monad *)
type 'a user

(** The carrier of the kernel monad *)
type 'a kernel

type t =
  | Abstract
  | Numeral of int
  | Boolean of bool
  | Quoted of string
  | Constructor of Name.t * t option
  | Tuple of t list
  | ClosureUser of (t -> t user)
  | ClosureKernel of (t -> t kernel)
  | Runner of cooperation Name.Map.t
  | Container of container

and world = World of t

and exc = Exception of Name.t * t

and sgn = Signal of Name.t * t

and cooperation = t -> t kernel

and container = (t * world -> t * world) Name.Map.t * world

(** The user monad structure *)
val user_return : 'a -> 'a user
val user_bind : 'a user -> ('a -> 'b user) -> 'b user

(** The kernel monad structure *)
val kernel_return : 'a -> 'a kernel
val kernel_bind : 'a kernel -> ('a -> 'b kernel) -> 'b kernel

(** Give a descriptive name of a value. *)
val name : t -> string

(** Give a descriptive name of a value, in plural. *)
val names : t -> string

(** The empty container *)
val pure_container : container

(** Pretty-print a value. *)
val print : ?max_level:Level.t -> t -> Format.formatter -> unit
