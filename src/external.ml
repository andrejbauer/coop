(** Foreign function interface. *)

exception Error of string

let error msg = raise (Error msg)

let as_int = function
  | Value.Numeral n -> n
  | Value.Constructor _ | Value.Boolean _ | Value.Quoted _ | Value.Tuple _ |
    Value.Closure _ | Value.Comodel _ | Value.Abstract ->
     error "integer expected"

let as_string = function
  | Value.Quoted s -> s
  | Value.Constructor _ | Value.Boolean _ | Value.Numeral _ | Value.Tuple _ |
    Value.Closure _ | Value.Comodel _ | Value.Abstract ->
     error "string expected"

(** Wrappers that convert OCaml data to Coop data. *)

let coop_unit = Value.Tuple []

let mk_ident s = Name.Ident (s, Name.Word)

let wrap_comodel w coops =
  let coops =
    List.fold_left
      (fun coops (x, f) -> Name.Map.add (mk_ident x) f coops)
      Name.Map.empty
      coops
  in
  Value.Comodel (w, coops)

let io =
  let r = Value.Val (coop_unit, Value.Abstract) in

  let print_value (v, _) =
    Format.printf "%t" (Value.print v) ;
    r

  and print_string (v, _) =
    let s = as_string v in
    Format.printf "%s" s ;
    r

  and read_string (_, _) =
    let s = Pervasives.input_line stdin in
    Value.Val (Value.Quoted s, Value.Abstract)

  and read_int (_, _) =
    try
      let k = Pervasives.read_int () in
      Value.Val (Value.Numeral k, Value.Abstract)
    with Failure _ -> error "malformed integer"
  in
  [ ("print_int", print_value);
    ("print_string", print_string);
    ("read_string", read_string);
    ("read_int", read_int);
    ("flush", fun (_, _) -> Format.printf "@." ; r)
  ]

(*
let wrap1 f =
  Value.Closure (fun v -> Value.Val (f v))
*)

let wrap_binary f =
  Value.Closure (fun v1 -> Value.Val (Value.Closure (fun v2 -> Value.Val (f v1 v2))))

let wrap_unary f = Value.Closure (fun v -> Value.Val (f v))

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
    ("io", wrap_comodel Value.Abstract io) ;
  ]

let lookup s = List.assoc_opt s externals
