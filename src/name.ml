(** Names of variables. *)

(** Kinds of names. *)
type fixity =
  | Word (** an ordinary word *)
  | Anonymous of int (** an anonymous name _ *)
  | Prefix (** prefix operator *)
  | Infix of Level.infix (** infix operator *)

(** An identifier. *)
type t = Ident of string * fixity

let equal i1 i2 = (i1 = i2)

(** Create a fresh anonymous name. *)
let anonymous =
  let k = ref (-1) in
  fun () -> (incr k ; Ident ("_", Anonymous !k))

(** Print an identifier. *)
let print ?(parentheses=true) x ppf =
  match x with
  | Ident (s, Word) -> Format.fprintf ppf "%s" s
  | Ident (_, Anonymous k) -> Format.fprintf ppf "_"
  | Ident (s, (Prefix|Infix _)) ->
     if parentheses then
       Format.fprintf ppf "( %s )" s
     else
       Format.fprintf ppf "%s" s
