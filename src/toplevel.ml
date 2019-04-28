(** Top-level processing. *)

type state =
  {
    desugar : Desugar.context; (** The desugaring state *)
    typecheck : Typecheck.context; (** The typechecking state *)
    runtime : Runtime.environment; (** The runtime environment *)
  }

let initial = {
    desugar = Desugar.initial;
    typecheck = Typecheck.initial;
    runtime = Runtime.initial
}

let exec_interactive {desugar; typecheck; runtime} =
  let e = Lexer.read_toplevel Parser.commandline () in
  let desugar, d = Desugar.toplevel desugar e in
  let typecheck, d = Typecheck.toplevel ~quiet:false typecheck d in
  let runtime = Eval.toplevel ~quiet:false runtime d in
  {desugar; typecheck; runtime}

let load_file ~quiet {desugar; typecheck; runtime} fn =
  let desugar, ds = Desugar.load desugar fn in
  let typecheck, ds = Typecheck.topfile ~quiet typecheck ds in
  let runtime = Eval.topfile ~quiet runtime ds in
  {desugar; typecheck; runtime}
