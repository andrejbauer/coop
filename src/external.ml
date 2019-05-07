(** Foreign function interface. *)

exception Error of string

let error msg = raise (Error msg)

let as_int = function
  | Value.Numeral n -> n
  | Value.Tuple _ | Value.Closure _ | Value.Comodel _ ->
     error "integer expected"

(*
let wrap1 f =
  Value.Closure (fun v -> Value.Val (f v))
*)

let wrap_binary f =
  Value.Closure (fun v1 -> Value.Val (Value.Closure (fun v2 -> Value.Val (f v1 v2))))

let wrap_unary f = Value.Closure (fun v -> Value.Val (f v))

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

let externals =
  [
    ("+", wrap_int_int_int ( + )) ;
    ("*", wrap_int_int_int ( * )) ;
    ("-", wrap_int_int_int ( - )) ;
    ("/", wrap_int_int_int (fun a b -> try a / b with Division_by_zero -> error "division by zero")) ;
    ("%", wrap_int_int_int (fun a b -> try a mod b with Division_by_zero -> error "division by zero")) ;
    ("~-", wrap_int_int ( ~- ))
  ]

let lookup s = List.assoc_opt s externals
