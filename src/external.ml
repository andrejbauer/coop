(** Foreign function interface. *)

let mk_ident s = Name.Ident (s, Name.Word)

(** Internal errors which do not happen to well-typed programs. *)
exception InternalError of string

let internal_error msg = raise (InternalError msg)

(** User exceptions that are also defined in the pervasives file.
    Make sure the names and types of values match! *)

let division_by_zero = Value.Exception (mk_ident "division_by_zero", Value.unit_val)

let malformed_integer = Value.Exception (mk_ident "malformed_integer", Value.unit_val)

(** Conversion of values to OCaml values. *)
let as_int = function
  | Value.Numeral n -> n
  | Value.(Constructor _ | Boolean _ | Quoted _ | Tuple _ |
    ClosureUser _ | ClosureKernel _ | Runner _ | Abstract | Container _) ->
     internal_error "integer expected"

let as_string = function
  | Value.Quoted s -> s
  | Value.(Constructor _ | Boolean _ | Numeral _ | Tuple _ |
    ClosureUser _ | ClosureKernel _ | Runner _ | Abstract | Container _) ->
     internal_error "string expected"

(** Wrappers that convert OCaml data to Coop data. *)

let wrap_container coops =
  let coops =
    List.fold_left
      (fun coops (x, f) -> Name.Map.add (mk_ident x) f coops)
      Name.Map.empty
      coops
  in
  Value.Container coops

let pure_container = []

let stdio_container =

  let print_string v =
    let s = as_string v in
    Format.printf "%s" s ;
    Value.unit_val

  and read_string _ =
    Format.printf "@." ;
    let s = Stdlib.input_line stdin in
    Value.Quoted s

  and read_int _ =
    try
      Format.printf "@." ;
      let k = Stdlib.read_int () in
      Value.Numeral k
    with Failure _ -> Value.coop_raise malformed_integer
  in
  [ ("print_string", print_string);
    ("read_string", read_string);
    ("read_int", read_int);
    ("flush", fun _ -> Format.printf "@." ; Value.unit_val)
  ]

let wrap_binary f =
  Value.ClosureUser (fun v1 ->
     Value.user_return (Value.ClosureUser
       (fun v2 -> Value.user_return (f v1 v2))))

let wrap_unary f =
  Value.ClosureUser (fun v -> Value.user_return (f v))

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
    ("/", wrap_int_int_int (fun a b -> try a / b with Division_by_zero -> Value.coop_raise division_by_zero)) ;
    ("%", wrap_int_int_int (fun a b -> try a mod b with Division_by_zero -> Value.coop_raise division_by_zero)) ;
    ("~-", wrap_int_int ( ~- )) ;
    ("<>",  wrap_int_int_bool ((<>) : int -> int -> bool)) ;
    ("<",  wrap_int_int_bool ((<) : int -> int -> bool)) ;
    (">",  wrap_int_int_bool ((>) : int -> int -> bool)) ;
    ("<=",  wrap_int_int_bool ((<=) : int -> int -> bool)) ;
    (">=",  wrap_int_int_bool ((>=) : int -> int -> bool)) ;
    ("^",  wrap_string_string_string (^)) ;
    ("string_of_int", wrap_int_string (string_of_int)) ;
    ("stdio", wrap_container stdio_container) ;
    ("pure", wrap_container pure_container) ;
  ]

let lookup s = List.assoc_opt s externals
