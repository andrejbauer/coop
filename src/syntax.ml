(** Type-checked syntax of Coop. *)

type signature = Name.Set.t

(** Expression type *)
type expr_ty =
  | Int
  | Product of expr_ty list
  | Arrow of expr_ty * comp_ty
  | ComodelTy of comodel_ty

(** Computation type *)
and comp_ty = CompTy of expr_ty * signature

(** Comodel *)
and comodel_ty = signature * expr_ty * signature

(** Patterns *)
type pattern =
  | PattAnonymous
  | PattVar
  | PattNumeral of int
  | PattTuple of pattern list

(** De Bruijn index *)
type index = int

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | Var of index
  | Numeral of int
  | Tuple of expr list
  | Lambda of comp
  | Comodel of expr * (Name.t * pattern * pattern * comp) list

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Val of expr
  | Let of pattern * comp * comp
  | Match of expr * (pattern * comp) list
  | Apply of expr * expr
  | Operation of Name.t * expr
  | Using of expr * comp * finally

and finally = pattern * pattern * comp

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * (Name.t * expr_ty) list * comp
  | TopComp of comp * expr_ty
  | DeclOperation of Name.t * expr_ty * expr_ty
  | External of Name.t * expr_ty * string

(** The unit type *)
let unit_ty = Product []

(** The empty signature *)
let empty_signature = Name.Set.empty

(** Make a pure computation type *)
let pure t = CompTy (t, empty_signature)

let op_ty t op =
  let sgn = Name.Set.add op Name.Set.empty in
  CompTy (t, sgn)

let pollute (CompTy (t, sgn1)) sgn2 =
  CompTy (t, Name.Set.union sgn1 sgn2)

let rec expr_subty t u =
  match t, u with

  | Int, Int -> true

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
     Name.Set.subset usgn1 tsgn1 && expr_eqtype t u && Name.Set.subset tsgn2 usgn2

  | Int,         (Product _ | Arrow _ | ComodelTy _)
  | Product _,   (Int | Arrow _ | ComodelTy _)
  | Arrow _,     (Int | Product _ | ComodelTy _)
  | ComodelTy _, (Int | Product _ | Arrow _) ->
     false

and comp_subty (CompTy (t1, drt1)) (CompTy (t2, drt2)) =
  Name.Set.subset drt1 drt2 && expr_subty t1 t2

and expr_eqtype t u =
  expr_subty t u && expr_subty u t

(** Pretty-print an expresion type *)
let rec print_expr_ty ?max_level ty ppf =
  match ty with

  | Int -> Format.fprintf ppf "int"

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

and print_comodel_ty (sgn1, w_ty, sgn2) ppf =
  Format.fprintf ppf "%t@ @@@ %t %s@ %t"
    (print_signature sgn1)
    (print_expr_ty ~max_level:Level.comodel_ty_world w_ty)
    (Print.char_darrow ())
    (print_signature sgn2)

and print_signature sgn ppf =
  let lst = List.sort Pervasives.compare (Name.Set.elements sgn) in
  Format.fprintf ppf "{%t}" (Print.sequence (Name.print ~parentheses:true) "," lst)
