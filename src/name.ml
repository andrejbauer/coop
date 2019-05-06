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

module Set =
struct
  module S = Set.Make(
                 struct
                   type nonrec t = t
                   let compare = Pervasives.compare
                 end)

  type elt = t
  type t = S.t

  let empty = S.empty
  let add = S.add
  let mem = S.mem
  let elements = S.elements
  let subset = S.subset
  let union = S.union

  let choose_diff s1 s2 = S.choose_opt (S.diff s1 s2)

end

module Map =
struct
  module M = Map.Make(
                 struct
                   type nonrec t = t
                   let compare = Pervasives.compare
                 end)

  type key = t
  type 'a t = 'a M.t

  let empty = M.empty

  let add = M.add

  let find = M.find_opt
end

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
