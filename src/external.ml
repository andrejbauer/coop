(** Foreign function interface. *)

(** Auxiliary function to make an identifier from a string. *)
let mk_ident s = Name.Ident (s, Name.Word)

(** Internal errors which do not happen to well-typed programs. *)
exception InternalError of string

let internal_error msg = raise (InternalError msg)

(** User exceptions that are also defined in the pervasives file.
    Make sure the names and types of values match! *)

let division_by_zero = Value.Exception (mk_ident "division_by_zero", Value.unit_val)

let malformed_integer = Value.Exception (mk_ident "malformed_integer", Value.unit_val)

let sys_error msg = Value.Exception (mk_ident "sys_error", Value.Quoted msg)

let end_of_file = Value.Exception (mk_ident "end_of_flie", Value.unit_val)

(** Conversion of values to OCaml values. *)

let as_int = function
  | Value.Numeral n -> n
  | Value.(Constructor _ | Boolean _ | Quoted _ | Tuple _ |
    ClosureUser _ | ClosureKernel _ | Runner _ | Abstract _ | Container _) ->
     internal_error "integer expected"

let as_string = function
  | Value.Quoted s -> s
  | Value.(Constructor _ | Boolean _ | Numeral _ | Tuple _ |
    ClosureUser _ | ClosureKernel _ | Runner _ | Abstract _ | Container _) ->
     internal_error "string expected"

let as_abstract = function
  | Value.Abstract a -> a
  | Value.(Constructor _ | Boolean _ | Numeral _ | Quoted _ | Tuple _ |
    ClosureUser _ | ClosureKernel _ | Runner _ | Container _) ->
     internal_error "abstract value expected"

let as_in_channel v =
  match as_abstract v with
  | Value.In_channel fh -> fh
  | Value.Out_channel _ -> internal_error "in_channel expected"

let as_out_channel v =
  match as_abstract v with
  | Value.Out_channel fh -> fh
  | Value.In_channel _ -> internal_error "out_channel expected"

(** Convert Ocaml functions to Coop functions *)

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

(** Wrappers that convert OCaml data to Coop data. *)

let wrap_container coops =
  let coops =
    List.fold_left
      (fun coops (x, f) -> Name.Map.add (mk_ident x) f coops)
      Name.Map.empty
      coops
  in
  Value.Container coops

(** Containers *)

let pure_container = []

let stdio_container =

  let print_int v =
    let k = as_int v in
    Format.printf "%d" k ;
    Value.unit_val

  and print_string v =
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
  [ ("print_int", print_int);
    ("print_string", print_string);
    ("read_string", read_string);
    ("read_int", read_int);
    ("flush_stdout", fun _ -> Format.printf "@." ; Value.unit_val)
  ]


let file_container =

  let coop_open_in v =
    let s = as_string v in
    try
      let fh = Stdlib.open_in s in
      Value.(Abstract (In_channel fh))
    with
    | Sys_error msg -> Value.coop_raise (sys_error msg)

  and coop_close_in v =
    try
      let fh = as_in_channel v in
      Stdlib.close_in fh ;
      Value.unit_val
    with Sys_error msg -> Value.coop_raise (sys_error msg)

  and coop_input_line v =
    try
      let fh = as_in_channel v in
      let s = Stdlib.input_line fh in
      Value.Quoted s
    with
    | Sys_error msg -> Value.coop_raise (sys_error msg)
    | End_of_file -> Value.coop_raise end_of_file

  and coop_open_out v =
    let s = as_string v in
    try
      let fh = Stdlib.open_out s in
      Value.(Abstract (Out_channel fh))
    with
    | Sys_error msg -> Value.coop_raise (sys_error msg)

  and coop_close_out v =
    try
      let fh = as_out_channel v in
      Stdlib.close_out fh ;
      Value.unit_val
    with
    | Sys_error msg -> Value.coop_raise (sys_error msg)

  and coop_output_string v =
    try
      let fh = as_out_channel v in
      wrap_unary (fun s -> let s = as_string s in Stdlib.output_string fh s ; Value.unit_val)
    with
    | Sys_error msg -> Value.coop_raise (sys_error msg)

  and coop_flush v =
    try
      let fh = as_out_channel v in
      Stdlib.flush fh ;
      Value.unit_val
    with
    | Sys_error msg -> Value.coop_raise (sys_error msg)
  in
  [ ("open_in", coop_open_in);
    ("close_in", coop_close_in);
    ("input_line", coop_input_line);
    ("open_out", coop_open_out);
    ("close_out", coop_close_out);
    ("output_string", coop_output_string);
    ("flush_out", coop_flush);
  ]

(** List of all externals *)

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
    ("file", wrap_container file_container) ;
    ("pure", wrap_container pure_container) ;
  ]

let lookup s = List.assoc_opt s externals
