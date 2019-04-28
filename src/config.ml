type pervasives =
  | PervasivesNone (** Do not load the pervasives file *)
  | PervasivesDefault (** Load the default pervasives file *)
  | PervasivesFile of string (** Load a specific pervasives file *)

let pervasives_file = ref PervasivesDefault

let interactive_shell = ref true

let wrapper = ref ["rlwrap"; "ledit"]

let max_boxes = ref 42

let columns = ref (Format.get_margin ())

let verbosity = ref 2

let ascii = ref false
