(** Configuration parameters that control how Terminus works. *)

(** How to load the pervasives file. *)
type pervasives =
  | PervasivesNone (** Do not load the pervasives file *)
  | PervasivesDefault (** Load the default pervasives file *)
  | PervasivesFile of string (** Load a specific pervasives file *)

(** The pervasives file to load. *)
val pervasives_file : pervasives ref

(** Should the interactive shell be started. *)
val interactive_shell : bool ref

(** List of command-line wrappers to try to use for command-line editing in interactive mode. *)
val wrapper : string list ref

(** How deeply should large expressions be printed. *)
val max_boxes : int ref

(** How many columns should be used for printing expressions. *)
val columns : int ref

(** How verbose should the output be. *)
val verbosity : int ref

(** Should we restrict to ASCII-only output. *)
val ascii : bool ref
