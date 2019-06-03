(** Type-checked syntax of Coop. *)

type operations = Name.Set.t

type signals = Name.Set.t

type signature = {
    sig_ops : operations ;
    sig_sgs : signals
  }

(** Primitive types *)
type primitive =
  | Empty
  | Int
  | Bool
  | String
  | Any

(** Expression type *)
type expr_ty =
  | Abstract of Name.t
  | Alias of Name.t
  | Datatype of Name.t
  | Primitive of primitive
  | Product of expr_ty list
  | Arrow of expr_ty * comp_ty
  | CohandlerTy of cohandler_ty
  | ShellTy of operations

(** Computation type *)
and comp_ty = { comp_ty : expr_ty ; comp_sig : signature }

(** Cohandler *)
and cohandler_ty = Name.Set.t * expr_ty * signature

(** The body of a datatype definition *)
type datatype = (Name.t * expr_ty option) list

(** Patterns *)
type pattern =
  | PattAnonymous
  | PattVar
  | PattNumeral of int
  | PattBoolean of bool
  | PattQuoted of string
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
  | Quoted of string
  | Constructor of Name.t * expr option
  | Tuple of expr list
  | Lambda of pattern * comp
  | Cohandler of (Name.t * pattern * pattern * comp) list
  | CohandlerTimes of expr * expr
  | CohandlerRename of expr * Name.t Name.Map.t

(** Computations *)
and comp = comp' Location.located
and comp' =
  | Val of expr
  | Let of pattern * comp * comp
  | LetRec of (pattern * comp) list * comp
  | Match of expr * (pattern * comp) list
  | Equal of expr * expr
  | Apply of expr * expr
  | Operation of Name.t * expr
  | Signal of Name.t * expr
  | Use of expr * expr * comp * finally

and finally = {
    fin_val : pattern * pattern * comp ;
    fin_signals : (Name.t * pattern * pattern * comp) list
}

(** Top-level commands. *)
type toplevel = toplevel' Location.located
and toplevel' =
  | TopLoad of toplevel list
  | TopLet of pattern * (Name.t * expr_ty) list * comp
  | TopLetRec of (pattern * comp) list * (Name.t * expr_ty) list
  | TopShell of comp * operations
  | TopComp of comp * expr_ty
  | DefineAbstract of Name.t
  | DefineAlias of Name.t * expr_ty
  | DefineDatatype of (Name.t * datatype) list
  | DeclareOperation of Name.t * expr_ty * expr_ty
  | DeclareSignal of Name.t * expr_ty
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
  { comp_ty = Primitive Empty ;
    comp_sig = { sig_ops = Name.Set.empty ;
                 sig_sgs = Name.Set.add sgl Name.Set.empty }
  }
let pollute {comp_ty; comp_sig=sgn1} sgn2 =
  { comp_ty ;
    comp_sig = { sig_ops = Name.Set.union sgn1.sig_ops sgn2.sig_ops ;
                 sig_sgs = Name.Set.union sgn1.sig_sgs sgn2.sig_sgs }
  }


(** Pretty-print a primitive type *)
let print_primitive p ppf =
  Format.fprintf ppf
  (match p with
  | Empty -> "empty"
  | Int -> "int"
  | Bool -> "bool"
  | String -> "string"
  | Any -> "any")


(** Pretty-print an expresion type *)
let rec print_expr_ty ?max_level ty ppf =
  match ty with

  | Abstract t -> Format.fprintf ppf "%t" (Name.print t)

  | Alias t -> Format.fprintf ppf "%t" (Name.print t)

  | Datatype t -> Format.fprintf ppf "%t" (Name.print t)

  | Primitive p -> print_primitive p ppf

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

  | CohandlerTy cmdl_ty -> print_cohandler_ty cmdl_ty ppf

  | ShellTy ops ->
     let ops = List.sort Pervasives.compare (Name.Set.elements ops) in
     Format.fprintf ppf "{%t}"
       (Print.sequence (Name.print ~parentheses:true) "," ops)

and print_comp_ty ?max_level {comp_ty; comp_sig} ppf =
  Print.print ?max_level ~at_level:Level.comp_ty ppf "%t@ !@ %t"
    (print_expr_ty ~max_level:Level.comp_ty_left comp_ty)
    (print_signature comp_sig)

and print_cohandler_ty (ops, w_ty, sgn2) ppf =
  let ops = List.sort Pervasives.compare (Name.Set.elements ops) in
  Format.fprintf ppf "{%t}@ @@@ %t %s@ %t"
    (Print.sequence (Name.print ~parentheses:true) "," ops)
    (print_expr_ty ~max_level:Level.cohandler_ty_world w_ty)
    (Print.char_darrow ())
    (print_signature sgn2)

and print_shell_ty ?max_level ops ppf =
  let ops = List.sort Pervasives.compare (Name.Set.elements ops) in
  Format.fprintf ppf "%t"
    (Print.sequence (Name.print ~parentheses:true) "," ops)

and print_signature {sig_ops; sig_sgs} ppf =
  let ops = List.sort Pervasives.compare (Name.Set.elements sig_ops)
  and sgs =  List.sort Pervasives.compare (Name.Set.elements sig_sgs) in
  Format.fprintf ppf "{%t%s%t}"
    (Print.sequence (Name.print ~parentheses:true) "," ops)
    (match ops, sgs with [], _ | _::_, [] -> "" | _::_, _::_ -> "; ")
    (Print.sequence (Name.print ~parentheses:true) "," sgs)

let print_datatype (t, cnstrs) ppf =
  let print_clause (cnstr, topt) ppf =
    match topt with
    | None ->
       Format.fprintf ppf "@[<h>| %t@]" (Name.print cnstr)
    | Some t ->
       Format.fprintf ppf "@[<h>| %t of %t@]"
         (Name.print cnstr)
         (print_expr_ty ~max_level:Level.product t)
  in
  Format.fprintf ppf "@[<hov -2>%t =@\n@[<hv>%t@]@]"
                 (Name.print t)
                 (Print.sequence print_clause "" cnstrs)

let rec print_datatypes dfs ppf =
  match dfs with
  | [] -> ()
  | [df] -> print_datatype df ppf
  | df :: dfs ->
     Format.fprintf ppf "%t@\n and %t" (print_datatype df) (print_datatypes dfs)
