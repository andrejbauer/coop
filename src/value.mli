(** Runtime values *)

(** Runtime value *)
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

(** The user monad carrier *)
and 'a user =
  | UserVal of 'a
  | UserException of exc
  | UserOperation of Name.t * t * (t -> 'a user) * (t -> 'a user) Name.Map.t

and 'a kernel_tree =
  | KernelVal of 'a * world
  | KernelException of exc * world
  | KernelSignal of sgn
  | KernelOperation of Name.t * t * (t -> 'a kernel_tree) * (t -> 'a kernel_tree) Name.Map.t

(** The kernel monad carrier *)
and 'a kernel = world -> 'a kernel_tree

and world = World of t

and exc = Exception of Name.t * t

and sgn = Signal of Name.t * t

and cooperation = t -> t kernel

and container = (t -> t) Name.Map.t

(** The user monad structure *)
val user_return : 'a -> 'a user
val user_bind : 'a user -> ('a -> 'b user) -> 'b user

(** The kernel monad structure *)
val kernel_return : 'a -> 'a kernel
val kernel_bind : 'a kernel -> ('a -> 'b kernel) -> 'b kernel

(** Native exception used by container cooperations *)
exception CoopException of exc

(** Give a descriptive name of a value. *)
val name : t -> string

(** Give a descriptive name of a value, in plural. *)
val names : t -> string

(** The empty container *)
val pure_container : container

(** Pretty-print a value. *)
val print : ?max_level:Level.t -> t -> Format.formatter -> unit
