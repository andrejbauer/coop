(** Foreign function interface. *)

exception Error of string

let error msg = raise (Error msg)

let as_int = function
  | Value.Numeral n -> n
  | Value.(Constructor _ | Boolean _ | Quoted _ | Tuple _ |
    ClosureUser _ | ClosureKernel _ | Runner _ | Abstract | Container _) ->
     error "integer expected"

let as_string = function
  | Value.Quoted s -> s
  | Value.(Constructor _ | Boolean _ | Numeral _ | Tuple _ |
    ClosureUser _ | ClosureKernel _ | Runner _ | Abstract | Container _) ->
     error "string expected"

(** Wrappers that convert OCaml data to Coop data. *)

let coop_unit = Value.Tuple []

let mk_ident s = Name.Ident (s, Name.Word)

let wrap_container coops w =
  let coops =
    List.fold_left
      (fun coops (x, f) -> Name.Map.add (mk_ident x) f coops)
      Name.Map.empty
      coops
  in
  Value.Container (coops, Value.World w)

let pure_container = []

let stdio_container =

  let print_value (v, _) =
    Format.printf "%t" (Value.print v) ;
    (coop_unit, Value.(World Abstract))

  and print_string (v, _) =
    let s = as_string v in
    Format.printf "%s" s ;
    (coop_unit, Value.(World Abstract))

  and read_string (_, _) =
    Format.printf "@." ;
    let s = Stdlib.input_line stdin in
    (Value.Quoted s, Value.(World Abstract))

  and read_int (_, _) =
    try
      Format.printf "@." ;
      let k = Stdlib.read_int () in
      (Value.Numeral k, Value.(World Abstract))
    with Failure _ -> error "malformed integer"
  in
  [ ("print_value", print_value);
    ("print_string", print_string);
    ("read_string", read_string);
    ("read_int", read_int);
    ("flush", fun (_, _) -> Format.printf "@." ; (coop_unit, Value.(World Abstract)))
  ]

let wrap_binary f =
  Value.ClosureUser (fun v1 -> Value.user_return (Value.ClosureUser (fun v2 -> Value.user_return (f v1 v2))))

let wrap_unary f = Value.ClosureUser (fun v -> Value.user_return (f v))

let wrap_string_string_string f =
  wrap_binary (fun v1 v2 ->
      let s1 = as_string v1
      and s2 = as_string v2 in
      let t = f s1 s2 in
      Value.Quoted t)

let wrap_int_int_int f =
  wrap_binary (fun v1 v2 ->
      let n1 = as_int v1
      and n2 = as_int v2 in
      let m = f n1 n2 in
      Value.Numeral m)

let wrap_int_int f =
  wrap_unary (fun v ->
      let n = as_int v in
      let m = f n in
      Value.Numeral m)

let wrap_int_string f =
  wrap_unary (fun v ->
      let n = as_int v in
      let m = f n in
      Value.Quoted m)

let wrap_int_int_bool f =
    wrap_binary (fun v1 v2 ->
      let n1 = as_int v1
      and n2 = as_int v2 in
      let b = f n1 n2 in
      Value.Boolean b)

let externals =
  [
    ("+", wrap_int_int_int ( + )) ;
    ("*", wrap_int_int_int ( * )) ;
    ("-", wrap_int_int_int ( - )) ;
    ("/", wrap_int_int_int (fun a b -> try a / b with Division_by_zero -> error "division by zero")) ;
    ("%", wrap_int_int_int (fun a b -> try a mod b with Division_by_zero -> error "division by zero")) ;
    ("~-", wrap_int_int ( ~- )) ;
    ("<>",  wrap_int_int_bool ((<>) : int -> int -> bool)) ;
    ("<",  wrap_int_int_bool ((<) : int -> int -> bool)) ;
    (">",  wrap_int_int_bool ((>) : int -> int -> bool)) ;
    ("<=",  wrap_int_int_bool ((<=) : int -> int -> bool)) ;
    (">=",  wrap_int_int_bool ((>=) : int -> int -> bool)) ;
    ("^",  wrap_string_string_string (^)) ;
    ("string_of_int", wrap_int_string (string_of_int)) ;
    ("stdio", wrap_container stdio_container Value.Abstract) ;
    ("pure", wrap_container pure_container Value.Abstract) ;
  ]

let lookup s = List.assoc_opt s externals
