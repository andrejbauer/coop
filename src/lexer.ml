(** Lexing with support for UTF8 characers. *)

(** Reserved words. *)
let reserved = [
  ("and", Parser.AND) ;
  ("as", Parser.AS) ;
  ("bool", Parser.BOOL) ;
  ("comodel", Parser.COMODEL) ;
  ("else", Parser.ELSE) ;
  ("empty", Parser.EMPTY) ;
  ("end", Parser.END) ;
  ("external", Parser.EXTERNAL) ;
  ("false", Parser.FALSE) ;
  ("finally", Parser.FINALLY) ;
  ("fun", Parser.FUN) ;
  ("if", Parser.IF) ;
  ("in", Parser.IN) ;
  ("int", Parser.INT) ;
  ("let", Parser.LET) ;
  ("load", Parser.LOAD) ;
  ("match", Parser.MATCH) ;
  ("of", Parser.OF) ;
  ("operation", Parser.OPERATION) ;
  ("rec", Parser.REC) ;
  ("signal", Parser.SIGNAL);
  ("string", Parser.STRING);
  ("then", Parser.THEN) ;
  ("true", Parser.TRUE) ;
  ("type", Parser.TYPE) ;
  ("unit", Parser.UNIT) ;
  ("using", Parser.USING) ;
  ("val", Parser.VAL) ;
  ("with", Parser.WITH) ;
]

let name =
  [%sedlex.regexp? (('_' | lowercase),
                 Star ('_' | alphabetic
                      | 185 | 178 | 179 | 8304 .. 8351 (* sub-/super-scripts *)
                      | '0'..'9' | '\'')) | math]

let constructor =
  [%sedlex.regexp? (uppercase,
                 Star ('_' | alphabetic
                      | 185 | 178 | 179 | 8304 .. 8351 (* sub-/super-scripts *)
                      | '0'..'9' | '\'')) | math]

let digit = [%sedlex.regexp? '0'..'9']
let numeral = [%sedlex.regexp? Opt '-', Plus digit]

let symbolchar = [%sedlex.regexp?  ('!' | '$' | '%' | '&' | '*' | '+' | '-' | '.' | '/' | ':' | '<' | '=' | '>' | '?' | '@' | '^' | '|' | '~')]

let prefixop = [%sedlex.regexp? ('~' | '?' | '!'), Star symbolchar ]
let infixop0 = [%sedlex.regexp? ":=" | "<-" ]
let infixop1 = [%sedlex.regexp? ('=' | '<' | '>' | '|' | '&' | '$'), Star symbolchar]
let infixop2 = [%sedlex.regexp? ('@' | '^'), Star symbolchar ]
let infixop3 = [%sedlex.regexp? ('+' | '-'), Star symbolchar ]
let infixop4 = [%sedlex.regexp? ('*' | '/' | '%'), Star symbolchar ]
let infixop5 = [%sedlex.regexp? "**", Star symbolchar ]

let start_longcomment = [%sedlex.regexp? "(*"]
let end_longcomment= [%sedlex.regexp? "*)"]

let newline = [%sedlex.regexp? ('\n' | '\r' | "\n\r" | "\r\n")]
let hspace  = [%sedlex.regexp? (' ' | '\t' | '\r')]

let quoted_string = [%sedlex.regexp? '"', (Star (Compl '"' | ("\\", any))), '"']

let update_eoi ({ Ulexbuf.pos_end; line_limit;_ } as lexbuf) =
  match line_limit with None -> () | Some line_limit ->
    if pos_end.Lexing.pos_lnum >= line_limit
    then Ulexbuf.reached_end_of_input lexbuf

let loc_of lex = Location.make lex.Ulexbuf.pos_start lex.Ulexbuf.pos_end

let safe_int_of_string lexbuf =
  let s = Ulexbuf.lexeme lexbuf in
  try
    int_of_string s
  with
    Invalid_argument _ -> Ulexbuf.error ~loc:(loc_of lexbuf) (Ulexbuf.BadNumeral s)

let rec token ({ Ulexbuf.end_of_input;_ } as lexbuf) =
  if end_of_input then Parser.EOF else token_aux lexbuf

and token_aux ({ Ulexbuf.stream;_ } as lexbuf) =
  let f () = Ulexbuf.update_pos lexbuf in
  match%sedlex stream with
  | newline                  -> f (); Ulexbuf.new_line lexbuf; token_aux lexbuf
  | start_longcomment        -> f (); comments 0 lexbuf
  | Plus hspace              -> f (); token_aux lexbuf
  | quoted_string            -> f ();
     let s = Ulexbuf.lexeme lexbuf in
     let l = String.length s in
     let n = ref 0 in
     String.iter (fun c -> if c = '\n' then incr n) s;
     Ulexbuf.new_line ~n:!n lexbuf;
     let s =
       try
         Scanf.unescaped (String.sub s 1 (l - 2))
       with Scanf.Scan_failure _ ->
         Format.printf "STRING IS [%s]@." s ;
         Ulexbuf.error ~loc:(loc_of lexbuf) Ulexbuf.MalformedQuotedString
     in
     Parser.QUOTED_STRING s
  | '|'                      -> f (); Parser.BAR
  | '_'                      -> f (); Parser.UNDERSCORE
  | '('                      -> f (); Parser.LPAREN
  | ')'                      -> f (); Parser.RPAREN
  | '{'                      -> f (); Parser.LBRACE
  | '}'                      -> f (); Parser.RBRACE
  | '@'                      -> f (); Parser.AT (Location.locate ~loc:(loc_of lexbuf) (Name.Ident("@", Name.Infix Level.Infix2)))
  | '!'                      -> f (); Parser.BANG (Location.locate ~loc:(loc_of lexbuf) (Name.Ident("!", Name.Prefix)))
  | '*' | 215                -> f (); Parser.STAR (Location.locate ~loc:(loc_of lexbuf)
                                                     (Name.Ident(Ulexbuf.lexeme lexbuf, Name.Infix Level.Infix4)))
  | ','                      -> f (); Parser.COMMA
  | ':'                      -> f (); Parser.COLON
  | ';'                      -> f (); Parser.SEMI
  | ";;"                     -> f (); Parser.SEMISEMI
  | '='                      -> f (); Parser.EQUAL (Location.locate ~loc:(loc_of lexbuf) (Name.Ident("=", Name.Infix Level.Infix1)))
  | "->" | 8594 | 10230      -> f (); Parser.ARROW
  | "=>" | 8658              -> f (); Parser.DARROW
  | ":*:" | 8855             -> f (); Parser.OTIMES

  (* We record the location of operators here because menhir cannot handle %infix and
     mark_location simultaneously, it seems. *)
  | prefixop                 -> f (); let op = Name.Ident (Ulexbuf.lexeme lexbuf, Name.Prefix) in
                                      let op = Location.locate ~loc:(loc_of lexbuf) op in
                                      Parser.PREFIXOP op

  | infixop0                 -> f (); let op = Name.Ident (Ulexbuf.lexeme lexbuf, Name.Infix Level.Infix0) in
                                      let op = Location.locate ~loc:(loc_of lexbuf) op in
                                      Parser.INFIXOP0 op

  | infixop1                 -> f (); let op = Name.Ident (Ulexbuf.lexeme lexbuf, Name.Infix Level.Infix1) in
                                      let op = Location.locate ~loc:(loc_of lexbuf) op in
                                      Parser.INFIXOP1 op

  | infixop2                 -> f (); let op = Name.Ident (Ulexbuf.lexeme lexbuf, Name.Infix Level.Infix2) in
                                      let op = Location.locate ~loc:(loc_of lexbuf) op in
                                      Parser.INFIXOP2 op

  | infixop3                 -> f (); let op = Name.Ident (Ulexbuf.lexeme lexbuf, Name.Infix Level.Infix3) in
                                      let op = Location.locate ~loc:(loc_of lexbuf) op in
                                      Parser.INFIXOP3 op

  (* Comes before infixop4 because ** matches the infixop4 pattern too *)
  | infixop5                 -> f (); let op = Name.Ident (Ulexbuf.lexeme lexbuf, Name.Infix Level.Infix5) in
                                      let op = Location.locate ~loc:(loc_of lexbuf) op in
                                      Parser.INFIXOP5 op

  | infixop4                 -> f (); let op = Name.Ident (Ulexbuf.lexeme lexbuf, Name.Infix Level.Infix4) in
                                      let op = Location.locate ~loc:(loc_of lexbuf) op in
                                      Parser.INFIXOP4 op

  | eof                      -> f (); Parser.EOF


  | constructor              -> f ();
    let n = Ulexbuf.lexeme lexbuf in
    Parser.CONSTRUCTOR (Name.Ident (n, Name.Word))

  | name                     -> f ();
    let n = Ulexbuf.lexeme lexbuf in
    begin try List.assoc n reserved
    with Not_found -> Parser.NAME (Name.Ident (n, Name.Word))
    end

  | numeral                  -> f (); let k = safe_int_of_string lexbuf in Parser.NUMERAL k

  | any -> f ();
     let w = Ulexbuf.lexeme lexbuf in
     let loc = loc_of lexbuf in
     Ulexbuf.error ~loc (Ulexbuf.Unexpected w)
  | _ -> assert false

and comments level ({ Ulexbuf.stream;_ } as lexbuf) =
  match%sedlex stream with
  | end_longcomment ->
    if level = 0 then
      begin Ulexbuf.update_pos lexbuf; token lexbuf end
    else
      comments (level-1) lexbuf

  | start_longcomment -> comments (level+1) lexbuf
  | '\n'        -> Ulexbuf.new_line lexbuf; comments level lexbuf
  | eof         -> Ulexbuf.error ~loc:(loc_of lexbuf) Ulexbuf.UnclosedComment
  | any         -> comments level lexbuf
  | _           -> assert false


(** run a menhir parser with a sedlexer on a t *)
(* the type of run is also:  *)
(* (t -> 'a) -> ('a, 'b) MenhirLib.Convert.traditional -> t -> 'b *)
let run
    (lexer : Ulexbuf.t -> 'a)
    (parser : (Lexing.lexbuf -> 'a) -> Lexing.lexbuf -> 'b)
    (lexbuf : Ulexbuf.t) : 'b =
  let lexer () =
    let token = lexer lexbuf in
    (token, lexbuf.Ulexbuf.pos_start, lexbuf.Ulexbuf.pos_end) in
  let parser = MenhirLib.Convert.Simplified.traditional2revised parser in
  try
    parser lexer
  with
  | Parser.Error ->
     let w = Ulexbuf.lexeme lexbuf in
     let loc = loc_of lexbuf in
     Ulexbuf.error ~loc (Ulexbuf.Unexpected w)
  | Sedlexing.MalFormed ->
     let loc = loc_of lexbuf in
     Ulexbuf.error ~loc Ulexbuf.MalformedUTF8
  (* | Sedlexing.InvalidCodepoint _ -> *)
  (*    assert false (\* Shouldn't happen with UTF8 *\) *)


let read_file parse fn =
  try
    let fh = open_in fn in
    let lex = Ulexbuf.from_channel ~fn fh in
    try
      let terms = run token parse lex in
      close_in fh;
      terms
    with
    (* Close the file in case of any parsing errors. *)
      Ulexbuf.Error err -> close_in fh; raise (Ulexbuf.Error err)
  with
  (* Any errors when opening or closing a file are fatal. *)
    Sys_error msg -> raise (Ulexbuf.error ~loc:Location.Nowhere (Ulexbuf.SysError msg))


let read_toplevel parse () =
  let all_white str =
    let n = String.length str in
    let rec fold k =
      k >= n ||
      (str.[k] = ' ' || str.[k] = '\n' || str.[k] = '\t') && fold (k+1)
    in
    fold 0
  in

  let rec read_more prompt acc =
    print_string prompt ;
    let str = read_line () in
    if all_white str
    then read_more prompt acc
    else acc ^ "\n" ^ str
  in

  let str = read_more "coop> " "" in
  let lex = Ulexbuf.from_string (str ^ "\n") in
  run token parse lex

let read_string parse s =
  let lex = Ulexbuf.from_string s in
  run token parse lex
