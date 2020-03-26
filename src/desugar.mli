(** The desugaring phase converts [Sugared] syntax to [Desugared] syntax. It
   performs the following transformations.

   The programmer may freely mix (effect-free) expressions and (effectful)
   computations. The desugaring phase separtes them and hoists computations
   which appear inside expressions into outer `let`-bindings, as necessary.

   The desugaring phase also keeps track of known identifiers, resources,
   signals and type names, and makes sure they do not get mixed up or shadowed.
*)

(** Desugaring errors *)
type desugar_error

(** A desugaring context is a list of known identifiers, which is used to
   compute de Bruijn indices. *)
type context

(** The initial empty context *)
val initial : context

(** The exception signalling a desugaring error*)
exception Error of desugar_error Location.located

(** Print desugaring error. *)
val print_error : desugar_error -> Format.formatter -> unit

(** Load a file and desugar it. *)
val load : context -> string -> context * Desugared.toplevel list

(** Desugar a toplevel. *)
val toplevel : context -> Sugared.toplevel -> context * Desugared.toplevel
