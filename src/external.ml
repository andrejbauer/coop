(** Foreign function interface. *)

exception Error of string

let error msg = raise (Error msg)

let as_int = function
  | Value.Numeral n -> n
  | Value.Constructor _ | Value.Boolean _ | Value.String _ | Value.Tuple _ |
    Value.Closure _ | Value.Comodel _ ->
     error "integer expected"

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

let os =
  [ ("print",
     (fun (v, _) -> Format.printf "%t@." (Value.print v) ;
                    Value.Val (coop_unit, coop_unit)))
  ]

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
    ("=",  wrap_int_int_bool ((=) : int -> int -> bool)) ;
    ("<>",  wrap_int_int_bool ((<>) : int -> int -> bool)) ;
    ("<",  wrap_int_int_bool ((<) : int -> int -> bool)) ;
    (">",  wrap_int_int_bool ((>) : int -> int -> bool)) ;
    ("<=",  wrap_int_int_bool ((<=) : int -> int -> bool)) ;
    (">=",  wrap_int_int_bool ((>=) : int -> int -> bool)) ;
    ("os", wrap_comodel coop_unit os) ;
  ]

let lookup s = List.assoc_opt s externals
