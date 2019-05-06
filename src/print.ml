(** Support for pretty-printing and user messages. *)

(** Print a message with given verbosity level. *)
let message ~verbosity =
  if verbosity <= !Config.verbosity then
    fun fmt -> Format.eprintf (fmt ^^ "@.")
  else
    Format.ifprintf Format.err_formatter

(** Report an error. *)
let error fmt = message ~verbosity:1 fmt

(** Report a warning. *)
let warning fmt = message ~verbosity:2 ("Warning: " ^^ fmt)

(** Print an expression, possibly parenthesized. *)
let print ?(at_level=Level.no_parens) ?(max_level=Level.highest) ppf =
  if Level.parenthesize ~at_level ~max_level then
    fun fmt -> Format.fprintf ppf ("(" ^^ fmt ^^ ")")
  else
    Format.fprintf ppf

(** Print a sequence with given separator and printer. *)
let sequence print_u separator us ppf =
  match us with
    | [] -> ()
    | [u] -> print_u u ppf
    | u :: ((_ :: _) as us) ->
      print_u u ppf ;
      List.iter (fun u -> print ppf "%s@ " separator ; print_u u ppf) us

(** Unicode and ascii versions of symbols. *)

let char_arrow () = if !Config.ascii then "->" else "→"
let char_darrow () = if !Config.ascii then "=>" else "⇒"
let char_times () = if !Config.ascii then "*" else "×"
