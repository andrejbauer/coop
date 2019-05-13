(** Type-checked syntax of Coop. *)

type signature = {
    sig_ops : Name.Set.t ;
    sig_sgs : Name.Set.t
  }

(** Expression type *)
type expr_ty =
  | TyAbbreviation of Name.t
  | TyDatatype of Name.t
  | SignalTy
  | Int
  | Bool
  | Product of expr_ty list
  | Arrow of expr_ty * comp_ty
  | ComodelTy of comodel_ty

(** Computation type *)
and comp_ty = { comp_ty : expr_ty ; comp_sig : signature }

(** Comodel *)
and comodel_ty = Name.Set.t * expr_ty * signature

(** The body of a datatype definition *)
type ty_definition =
  | TydefAbbreviation of expr_ty
  | TydefDatatype of (Name.t * expr_ty option) list

(** Patterns *)
type pattern =
  | PattAnonymous
  | PattVar
  | PattNumeral of int
  | PattBoolean of bool
  | PattConstructor of Name.t * pattern option
  | PattTuple of pattern list

(** De Bruijn index *)
type index = int

(** Expressions *)
type expr = expr' Location.located
and expr' =
  | Var of index
  | Numeral of int
  | Boolean of bool
  | Constructor of Name.t * expr option
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
  | TypeDefinition of (Name.t * ty_definition) list
  | DeclOperation of Name.t * expr_ty * expr_ty
  | DeclSignal of Name.t * expr_ty
  | External of Name.t * expr_ty * string

(** The unit type *)
let unit_ty = Product []

(** The empty signature *)
let empty_signature = { sig_ops = Name.Set.empty; sig_sgs = Name.Set.empty }

(** Make a pure computation type *)
let pure t = { comp_ty = t ; comp_sig = empty_signature }

let operation_ty t op =
  { comp_ty = t ;
    comp_sig = { sig_ops = Name.Set.add op Name.Set.empty ;
                 sig_sgs = Name.Set.empty }
  }

let signal_ty sgl =
  { comp_ty = SignalTy ;
    comp_sig = { sig_ops = Name.Set.empty ;
                 sig_sgs = Name.Set.add sgl Name.Set.empty }
  }
let pollute {comp_ty; comp_sig=sgn1} sgn2 =
  { comp_ty ;
    comp_sig = { sig_ops = Name.Set.union sgn1.sig_ops sgn2.sig_ops ;
                 sig_sgs = Name.Set.union sgn1.sig_sgs sgn2.sig_sgs }
  }

(** Pretty-print an expresion type *)
let rec print_expr_ty ?max_level ty ppf =
  match ty with

  | TyAbbreviation t -> Format.fprintf ppf "%t" (Name.print t)

  | TyDatatype t -> Format.fprintf ppf "%t" (Name.print t)

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

and print_comp_ty ?max_level {comp_ty; comp_sig} ppf =
  Print.print ?max_level ~at_level:Level.comp_ty ppf "%t@ !@ %t"
    (print_expr_ty ~max_level:Level.comp_ty_left comp_ty)
    (print_signature comp_sig)

and print_comodel_ty (ops, w_ty, sgn2) ppf =
  let ops = List.sort Pervasives.compare (Name.Set.elements ops) in
  Format.fprintf ppf "{%t}@ @@@ %t %s@ %t"
    (Print.sequence (Name.print ~parentheses:true) "," ops)
    (print_expr_ty ~max_level:Level.comodel_ty_world w_ty)
    (Print.char_darrow ())
    (print_signature sgn2)

and print_signature {sig_ops; sig_sgs} ppf =
  let lst =
    List.sort Pervasives.compare (Name.Set.elements sig_ops @ Name.Set.elements sig_sgs)
  in
  Format.fprintf ppf "{%t}"
    (Print.sequence (Name.print ~parentheses:true) "," lst)

let print_ty_definition ty_def ppf =
  let print_clause (cnstr, topt) ppf =
    match topt with
    | None ->
       Name.print cnstr ppf
    | Some t ->
       Format.fprintf ppf "@[<h>%t of %t@]"
         (Name.print cnstr)
         (print_expr_ty ~max_level:Level.product t)
  in
  match ty_def with

  | (t, TydefAbbreviation abbrev) ->
     Format.fprintf ppf "@[<hov>%t@ =@ %t@]"
                    (Name.print t)
                    (print_expr_ty abbrev)

  | (t, TydefDatatype data) ->
     Format.fprintf ppf "@[<hov>%t@ =@ %t@]"
                    (Name.print t)
                    (Print.sequence print_clause " |" data)
