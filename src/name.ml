(** Names of variables. *)

(** Kinds of names. *)
type fixity =
  | Word (** an ordinary word *)
  | Anonymous of int (** an anonymous name _ *)
  | Prefix (** prefix operator *)
  | Infix of Level.infix (** infix operator *)

(** An identifier. *)
type t = Ident of string * fixity

(** The name of an operation. *)
type op = Op of t

let equal i1 i2 = (i1 = i2)

(** Sets of identifiers *)
module Idset =
struct
  module I = Set.Make(
                 struct
                   type nonrec t = t
                   let compare = Stdlib.compare
                 end)

  type elt = t
  type t = I.t

  let empty = I.empty
  let is_empty = I.is_empty
  let remove = I.remove
  let add = I.add
  let mem = I.mem
  let elements = I.elements
  let subset = I.subset
  let union = I.union
  let inter = I.inter
  let fold = I.fold

  let diff s1 s2 = I.diff s1 s2
end

(** Sets of operation names *)
module Opset =
struct
  module O = Set.Make(
                 struct
                   type nonrec t = op
                   let compare = Stdlib.compare
                 end)

  type elt = op
  type t = O.t

  let empty = O.empty
  let is_empty = O.is_empty
  let remove = O.remove
  let add = O.add
  let mem = O.mem
  let elements = O.elements
  let subset = O.subset
  let union = O.union
  let inter = O.inter
  let fold = O.fold

  let diff s1 s2 = O.diff s1 s2
end

module Map =
struct
  module M = Map.Make(
                 struct
                   type nonrec t = t
                   let compare = Stdlib.compare
                 end)

  type key = t
  type 'a t = 'a M.t

  let empty = M.empty
  let mem = M.mem
  let add = M.add
  let find = M.find_opt
  let merge = M.merge
  let fold = M.fold
  let map = M.map
end

(** Create a fresh anonymous name. *)
let anonymous =
  let k = ref (-1) in
  fun () -> (incr k ; Ident ("_", Anonymous !k))

(** Print an identifier. *)
let print ?(parentheses=true) x ppf =
  match x with
  | Ident (s, Word) -> Format.fprintf ppf "%s" s
  | Ident (_, Anonymous k) -> Format.fprintf ppf "_%d" k
  | Ident (s, (Prefix|Infix _)) ->
     if parentheses then
       Format.fprintf ppf "( %s )" s
     else
       Format.fprintf ppf "%s" s
