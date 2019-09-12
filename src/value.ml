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
