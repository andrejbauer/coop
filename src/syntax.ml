(** Type-checked syntax of Coop. *)

type signature = {
    sig_ops : Name.Set.t ;
    sig_sgs : Name.Set.t
  }

(** Expression type *)
type expr_ty =
  | SignalTy
  | Int
  | Bool
  | Product of expr_ty list
  | Arrow of expr_ty * comp_ty
  | ComodelTy of comodel_ty

(** Computation type *)
and comp_ty =
  | CompTy of expr_ty * signature

(** Comodel *)
and comodel_ty = Name.Set.t * expr_ty * signature

(** Patterns *)
type pattern =
  | PattAnonymous
  | PattVar
  | PattNumeral of int
  | PattBoolean of bool
  | PattTuple of pattern list

(** De Bruijn index *)
type index = int

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | Var of index
  | Numeral of int
  | Boolean of bool
  | Tuple of expr list
  | Lambda of pattern * comp
  | Comodel of (Name.t * pattern * pattern * comp) list

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Val of expr
  | Let of pattern * comp * comp
  | Match of expr * (pattern * comp) list
  | Apply of expr * expr
  | Operation of Name.t * expr
  | Signal of Name.t * expr
  | Using of expr * expr * comp * finally

and finally = {
    fin_val : pattern * pattern * comp ;
    fin_signals : (Name.t * pattern * pattern * comp) list
}

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * (Name.t * expr_ty) list * comp
  | TopComp of comp * expr_ty
  | DeclOperation of Name.t * expr_ty * expr_ty
  | DeclSignal of Name.t * expr_ty
  | External of Name.t * expr_ty * string

(** The unit type *)
let unit_ty = Product []

(** The empty signature *)
let empty_signature = { sig_ops = Name.Set.empty; sig_sgs = Name.Set.empty }

(** Is the first signature a subsignature of the second one? *)
let subsignature {sig_ops=ops1; sig_sgs=sgs1} {sig_ops=ops2; sig_sgs=sgs2} =
  Name.Set.subset ops1 ops2 && Name.Set.subset sgs1 sgs2

let join_signature {sig_ops=ops1; sig_sgs=sgs1} {sig_ops=ops2; sig_sgs=sgs2} =
  let sig_ops = Name.Set.union ops1 ops2
  and sig_sgs = Name.Set.union sgs1 sgs2 in
  {sig_ops; sig_sgs}

let meet_signature {sig_ops=ops1; sig_sgs=sgs1} {sig_ops=ops2; sig_sgs=sgs2} =
  let sig_ops = Name.Set.inter ops1 ops2
  and sig_sgs = Name.Set.inter sgs1 sgs2 in
  {sig_ops; sig_sgs}

(** Make a pure computation type *)
let pure t = CompTy (t, empty_signature)

let operation_ty t op =
 let sgn = { sig_ops = Name.Set.add op Name.Set.empty ;
              sig_sgs = Name.Set.empty }
  in
  CompTy (t, sgn)

let signal_ty sgl =
 let sgn = { sig_ops = Name.Set.empty ;
             sig_sgs = Name.Set.add sgl Name.Set.empty }
  in
  CompTy (SignalTy, sgn)

let pollute (CompTy (t, sgn1)) sgn2 =
  let sgn = { sig_ops = Name.Set.union sgn1.sig_ops sgn2.sig_ops ;
              sig_sgs = Name.Set.union sgn1.sig_sgs sgn2.sig_sgs }
  in
  CompTy (t, sgn)

let rec expr_subty t u =
  match t, u with

  | SignalTy, _ -> true

  | _, SignalTy -> false

  | Int, Int -> true

  | Bool, Bool -> true

  | Product ts, Product us ->
     let rec fold ts us =
       match ts, us with
       | [], [] -> true
       | t :: ts, u :: us -> expr_subty t u && fold ts us
       | [], _::_ | _::_, [] -> false
     in
     fold ts us

  | Arrow (t1, t2), Arrow (u1, u2) ->
     expr_subty u1 t1 && comp_subty t2 u2

  | ComodelTy (tsgn1, t, tsgn2), ComodelTy (usgn1, u, usgn2) ->
     Name.Set.subset usgn1 tsgn1 && expr_eqtype t u && subsignature  tsgn2 usgn2

  | Int,         (Bool | Product _ | Arrow _ | ComodelTy _)
  | Bool,        (Int | Product _ | Arrow _ | ComodelTy _)
  | Product _,   (Int | Bool | Arrow _ | ComodelTy _)
  | Arrow _,     (Int | Bool | Product _ | ComodelTy _)
  | ComodelTy _, (Int | Bool | Product _ | Arrow _) ->
     false

and comp_subty (CompTy (t1, sig1)) (CompTy (t2, sig2)) =
  subsignature sig1 sig2 && expr_subty t1 t2

and expr_eqtype t u =
  expr_subty t u && expr_subty u t

(** Pretty-print an expresion type *)
let rec print_expr_ty ?max_level ty ppf =
  match ty with

  | SignalTy -> Format.fprintf ppf "signal"

  | Int -> Format.fprintf ppf "int"

  | Bool -> Format.fprintf ppf "bool"

  | Product [] -> Format.fprintf ppf "unit"

  | Product lst ->
     let st = " " ^ Print.char_times () in
     Print.print ?max_level ~at_level:Level.product ppf "%t"
       (Print.sequence (print_expr_ty ~max_level:Level.product_arg) st lst)

  | Arrow (t1, t2) ->
     Print.print ?max_level ~at_level:Level.arr ppf "%t@ %s@ %t"
       (print_expr_ty ~max_level:Level.arr_left t1)
       (Print.char_arrow ())
       (print_comp_ty ~max_level:Level.arr_right t2)

  | ComodelTy cmdl_ty -> print_comodel_ty cmdl_ty ppf

and print_comp_ty ?max_level (CompTy (t, sgn)) ppf =
  Print.print ?max_level ~at_level:Level.comp_ty ppf "%t@ !@ %t"
    (print_expr_ty ~max_level:Level.comp_ty_left t)
    (print_signature sgn)

and print_comodel_ty (ops, w_ty, sgn2) ppf =
  let ops = List.sort Pervasives.compare (Name.Set.elements ops) in
  Format.fprintf ppf "{%t}@ @@@ %t %s@ %t"
    (Print.sequence (Name.print ~parentheses:true) "," ops)
    (print_expr_ty ~max_level:Level.comodel_ty_world w_ty)
    (Print.char_darrow ())
    (print_signature sgn2)

and print_signature {sig_ops; sig_sgs} ppf =
  let lst = List.sort Pervasives.compare (Name.Set.elements sig_ops @ Name.Set.elements sig_sgs) in
  Format.fprintf ppf "{%t}"
    (Print.sequence (Name.print ~parentheses:true) "," lst)
