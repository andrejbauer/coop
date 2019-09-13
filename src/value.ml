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

and 'a user =
  | UserVal of 'a
  | UserException of exc
  | UserOperation of Name.t * t * (t -> 'a user) * (exc -> 'a user)

and 'a kernel_tree =
  | KernelVal of 'a * world
  | KernelException of exc * world
  | KernelSignal of sgn
  | KernelOperation of Name.t * t * (t -> 'a kernel_tree) * (exc -> 'a kernel_tree)

and 'a kernel = world -> 'a kernel_tree

and cooperation = t -> t kernel

and container = (t * world -> t * world) Name.Map.t * world

(** The user monad. *)
let user_return x = UserVal x

let rec user_bind r k =
  match r with
  | UserVal v -> k v
  | UserException _ as r -> r
  | UserOperation (op, u, l_val, l_exc) ->
     let l_val = (fun x -> let r = l_val x in user_bind r k)
     and l_exc = (fun e -> let r = l_exc e in user_bind r k) in
     UserOperation (op, u, l_val, l_exc)

(** The kernel monad *)
let kernel_return x w = KernelVal (x, w)

let kernel_bind k f w =
  let rec fold r f =
    match r with
    | KernelVal (v, w) -> f v w
    | KernelException _ as r -> r
    | KernelSignal _ as r -> r
    | KernelOperation (op, u, g, h) ->
       let g = (fun v -> let r = g v in fold r f)
       and h = (fun e -> let r = h e in fold r f) in
       KernelOperation (op, u, g, h)
  in
  match k w with
    | KernelVal (v, w') -> f v w'
    | KernelException _ as r  -> r
    | KernelSignal _ as r -> r
    | KernelOperation (op, u, g, h) ->
       let g = (fun v -> let r = g v in fold r f)
       and h = (fun e -> let r = h e in fold r f) in
       KernelOperation (op, u, g, h)

let name = function
  | Abstract -> "abstract value"
  | Numeral _ -> "integer"
  | Boolean _ -> "boolean"
  | Quoted _ -> "string"
  | Constructor _ -> "constructor"
  | Tuple _ -> "tuple"
  | ClosureUser _ -> "user function"
  | ClosureKernel _ -> "kernel function"
  | Runner _ -> "runner"
  | Container _ -> "container"

let names = function
  | Abstract -> "abstract values"
  | Numeral _ -> "integers"
  | Boolean _ -> "booleans"
  | Quoted _ -> "strings"
  | Constructor _ -> "constructors"
  | Tuple _ -> "tuples"
  | ClosureUser _ -> "user functions"
  | ClosureKernel _ -> "kernel functions"
  | Runner _ -> "runners"
  | Container _ -> "containers"

let pure_container = (Name.Map.empty, World Abstract)

let rec print ?max_level v ppf =
  match v with
  | Abstract -> Format.fprintf ppf "<abstr>"

  | Numeral k -> Format.fprintf ppf "%d" k

  | Boolean b -> Format.fprintf ppf "%b" b

  | Quoted s -> Format.fprintf ppf "\"%s\"" (String.escaped s)

  | Constructor (Name.Ident (_, Name.Prefix) as x, Some (Tuple [e])) ->
     Print.print ?max_level ~at_level:Level.prefix ppf "%t@ %t"
       (Name.print ~parentheses:false x)
       (print ~max_level:Level.prefix_arg e)

  | Constructor (Name.Ident (_, Name.Infix fixity) as x, Some (Tuple [e1; e2])) ->
     let lvl, lvl_left, lvl_right = Level.infix fixity in
     Print.print ?max_level ~at_level:lvl ppf "%t@ %t@ %t"
       (print ~max_level:lvl_left e1)
       (Name.print ~parentheses:false x)
       (print ~max_level:lvl_right e2)

  | Constructor (x, Some v) ->
     Print.print ?max_level ~at_level:Level.constr ppf "%t@ %t"
       (Name.print ~parentheses:true x)
       (print ~max_level:Level.constr_arg v)

  | Constructor (x, None) ->
     Name.print ~parentheses:true x ppf

  | Tuple lst ->
     Format.fprintf ppf "(%t)" (print_tuple lst)

  | ClosureUser _ -> Format.fprintf ppf "<fun>"

  | ClosureKernel _ -> Format.fprintf ppf "<funk>"

  | Runner _ -> Format.fprintf ppf "<runner>"

  | Container _ -> Format.fprintf ppf "<containers>"

and print_tuple lst ppf =
  Print.sequence (print ~max_level:Level.tuple_arg) "," lst ppf
