(* Infix operations a la OCaml *)
%token <Name.t Location.located> PREFIXOP INFIXOP0 INFIXOP1 EQUAL INFIXOP2 INFIXOP3 INFIXOP4 INFIXOP5 STAR BANG AT

(* Names *)
%token <Name.t> NAME
%token <Name.t> CONSTRUCTOR
%token UNDERSCORE

(* Primitive types *)
%token EMPTY INT UNIT BOOL STRING ANY

(* Parentheses & punctuations *)
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COLON ARROW DARROW SEMI SEMISEMI COMMA

(* Expressions and computations *)
%token <int> NUMERAL
%token BEGIN END
%token FALSE TRUE IF THEN ELSE
%token FUN FUNK
%token RUNNER
%token GETENV SETENV
%token LET REC IN
%token MATCH WITH BAR
%token USING RUN TRY FINALLY VAL
%token RAISE KILL

(* Toplevel commands *)

%token <string> QUOTED_STRING
%token LOAD
%token OPERATION
%token EXCEPTION SIGNAL OF
%token CONTAINER
%token EXTERNAL
%token TYPE AND

(* End of input token *)
%token EOF

(* Precedence and fixity of infix operators *)

%nonassoc INFIXOP0
%left     INFIXOP1 EQUAL
%right    INFIXOP2 AT
%left     INFIXOP3
%left     INFIXOP4 STAR
%right    INFIXOP5

%start <Sugared.toplevel list> file
%start <Sugared.toplevel> commandline

%%

(* Toplevel syntax *)

file:
  | f=filecontents EOF
    { f }


filecontents:
  |
    { [] }

  | d=topcomp
    { [d] }

  | d=toplevel SEMISEMI ds=filecontents
    { d :: ds }

  | d=topcomp  SEMISEMI ds=filecontents
    { d :: ds }

  | d=toplevel ds=filecontents_top
    { d :: ds }


filecontents_top:
  |
    { [] }

  | d=toplevel SEMISEMI ds=filecontents
    { d :: ds }

  | d=toplevel ds=filecontents_top
    { d :: ds }


commandline:
  | t=toplevel SEMISEMI? EOF
    { t }

  | t=topcomp SEMISEMI? EOF
    { t }


(* Toplevel computation *)
topcomp: mark_location(topcomp_) { $1 }
topcomp_:
  | c=term
    { Sugared.TopComp c }

(* Things that can be done at the toplevel, except for a computation. *)
toplevel: mark_location(toplevel_) { $1 }
toplevel_:
  | LOAD fn=QUOTED_STRING
    { Sugared.TopLoad fn }

  | LET bnd=let_binding
    { Sugared.TopLet bnd }

  | LET REC fs=separated_nonempty_list(AND, recursive_clause)
    { Sugared.TopLetRec fs }

  | CONTAINER c=infix_term
    { Sugared.TopContainer c }

  | OPERATION op=var_name COLON t1=prod_ty ARROW t2=ty
    { Sugared.DeclareOperation (op, t1, t2) }

  | EXCEPTION exc=var_name OF t=ty
    { Sugared.DeclareException (exc, t) }

  | SIGNAL sgl=var_name OF t=ty
    { Sugared.DeclareSignal (sgl, t) }

  | EXTERNAL x=var_name COLON t=ty EQUAL s=QUOTED_STRING
    { Sugared.External (x, t, s) }

  | TYPE x=var_name
    { Sugared.DefineAbstract x }

  | TYPE x=var_name EQUAL t=ty
    { Sugared.DefineAlias (x, t) }

  | TYPE lst=separated_list(AND, datatype)
    { Sugared.DefineDatatype lst }


(* Main syntax tree *)
term : mark_location(term_) { $1 }
term_:
  | e=infix_term_
    { e }

  | e=infix_term COLON t=ty
    { Sugared.Ascribe (e, t) }

  | FUN a=binder+ ARROW e=term
    { Sugared.Lambda (a, e) }

  | FUNK a=binder+ ARROW e=term
    { Sugared.Lambda (a, e) }

  | LET bnd=let_binding IN c=term
    { Sugared.Let (bnd, c) }

  | LET REC fs=separated_nonempty_list(AND, recursive_clause) IN c2=term
    { Sugared.LetRec (fs, c2) }

  | e1=infix_term SEMI e2=term
    { Sugared.Sequence (e1, e2) }

  | MATCH e=infix_term WITH LBRACE lst=match_clauses RBRACE
    { Sugared.Match (e, lst) }

  | IF e1=term THEN e2=term ELSE e3=term
    { Sugared.If (e1, e2, e3) }

  | RUNNER LBRACE lst=runner_clauses RBRACE AT t=ty
    { Sugared.Runner (t, lst) }

  | USING rnr=infix_term AT w=infix_term RUN c=term FINALLY LBRACE fin=finally RBRACE
    { Sugared.Run (rnr, w, c, fin) }

  | TRY c=term WITH LBRACE tr=trying RBRACE
    { Sugared.Try (c, tr) }

infix_term: mark_location(infix_term_) { $1 }
infix_term_:
  | e=app_term_
    { e }

  | e1=infix_term EQUAL e2=infix_term
    { Sugared.Equal (e1, e2) }

  | e2=infix_term oploc=infix e3=infix_term
    { let {Location.it=op; loc} = oploc in
      let op = Location.locate ~loc (Sugared.Var op) in
      let e1 = Location.locate ~loc (Sugared.Apply (op, e2)) in
      Sugared.Apply (e1, e3)
    }

app_term: mark_location(app_term_) { $1 }
app_term_:
  | e=prefix_term_
    { e }

  | e1=app_term e2=prefix_term
    { Sugared.Apply (e1, e2) }

  | SETENV e=prefix_term
    { Sugared.Setenv e }

  | KILL e=prefix_term
    { Sugared.Kill e }

  | RAISE e=prefix_term
    { Sugared.Raise e }

prefix_term: mark_location(prefix_term_) { $1 }
prefix_term_:
  | e=simple_term_
    { e }

  | oploc=prefix e2=prefix_term
    { let {Location.it=op; loc} = oploc in
      let op = Location.locate ~loc (Sugared.Var op) in
      Sugared.Apply (op, e2)
    }

(* simple_term : mark_location(simple_term_) { $1 } *)
simple_term_:
  | n=NUMERAL
    { Sugared.Numeral n }

  | FALSE
    { Sugared.False }

  | TRUE
    { Sugared.True }

  | cnstr=CONSTRUCTOR
    { Sugared.Constructor cnstr }

  | x=var_name
    { Sugared.Var x }

  | s=QUOTED_STRING
    { Sugared.Quoted s }

  | GETENV
    { Sugared.Getenv }

  | BEGIN c=term END
    { c.Location.it }

  | LPAREN es=separated_list(COMMA, term) RPAREN
                { match es with
                  | [e] -> e.Location.it
                  | [] | _::_ -> Sugared.Tuple es }

var_name:
  | NAME                     { $1 }
  | LPAREN op=infix RPAREN   { op.Location.it }
  | LPAREN op=prefix RPAREN  { op.Location.it }

%inline infix:
  | op=INFIXOP0    { op }
  | op=INFIXOP1    { op }
  | op=INFIXOP2    { op }
  | op=AT          { op }
  | op=INFIXOP3    { op }
  | op=INFIXOP4    { op }
  | op=STAR        { op }
  | op=INFIXOP5    { op }

%inline prefix:
  | op=PREFIXOP    { op }
  | op=BANG        { op }

match_clauses:
  | BAR lst=separated_nonempty_list(BAR, match_clause)
    { lst }

  | lst=separated_list(BAR, match_clause)
    { lst }

match_clause:
  | p=match_binder ARROW c=term
    { (p, c) }

runner_clauses:
  | BAR lst=separated_nonempty_list(BAR, runner_clause)
    { lst }

  | lst=separated_list(BAR, runner_clause)
    { lst }

runner_clause:
  | op=var_name px=binder ARROW c=term
    { (op, px, c) }

finally:
  | BAR lst=separated_nonempty_list(BAR, finally_clause)
    { lst }
  | lst=separated_list(BAR, finally_clause)
    { lst }

finally_clause:
  | VAL px=binder AT pw=binder ARROW t=term
    { Sugared.FinVal (px, pw, t) }

  | RAISE exc=var_name px=binder AT pw=binder ARROW c=term
    { Sugared.FinRaise (exc, px, pw, c) }

  | KILL sgl=var_name px=binder ARROW c=term
    { Sugared.FinKill (sgl, px, c) }

trying:
  | BAR lst=separated_nonempty_list(BAR, try_clause)
    { lst }
  | lst=separated_list(BAR, try_clause)
    { lst }

try_clause:
  | VAL px=binder ARROW t=term
    { Sugared.TryVal (px, t) }

  | RAISE exc=var_name px=binder ARROW c=term
    { Sugared.TryRaise (exc, px, c) }

  | KILL sgl=var_name px=binder ARROW c=term
    { Sugared.TryKill (sgl, px, c) }

binder:
  | p=simple_pattern
    { (p, None) }

  | pt=typed_binder
    { let (p,t) = pt in (p, Some t) }

typed_binder:
  | LPAREN p=pattern COLON t=ty RPAREN
    { (p, t) }

recursive_clause:
  | f=var_name px=typed_binder pxs=typed_binder* COLON t=ty EQUAL c=term
    { (f, px, pxs, t, c) }

annot:
  | COLON t = ty
    { t }

let_binding:
  | p=pattern t=annot? EQUAL e=term
    { Sugared.BindVal (p, t, e) }

  | f=var_name a=binder+ t=annot? EQUAL e=term
    { Sugared.BindFun (f, a, t, e) }

match_binder:
  | p=pattern
    { (p, None) }

  | LPAREN p=pattern COLON t=ty RPAREN
    { (p, Some t) }

pattern : mark_location(pattern_) { $1 }
pattern_:
  | p=simple_pattern_
    { p }

  | cnstr=CONSTRUCTOR p=simple_pattern
    { Sugared.PattConstructor (cnstr, Some p) }

simple_pattern: mark_location(simple_pattern_) { $1 }
simple_pattern_:
  | UNDERSCORE
    { Sugared.PattAnonymous }

  | x=var_name
    { Sugared.PattVar x }

  | k=NUMERAL
    { Sugared.PattNumeral k }

  | TRUE
    { Sugared.PattBoolean true }

  | FALSE
    { Sugared.PattBoolean false }

  | s=QUOTED_STRING
    { Sugared.PattQuoted s }

  | cnstr=CONSTRUCTOR
    { Sugared.PattConstructor (cnstr, None) }

  | LPAREN lst=separated_list(COMMA, pattern) RPAREN
    { match lst with
      | [p] -> p.Location.it
      | [] | _::_ -> Sugared.PattTuple lst }

ty: mark_location(ty_) { $1 }
ty_:
  | t=comp_ty_
    { t }

  | t1=comp_ty ARROW t2=ty
    { Sugared.Arrow (t1, t2) }

  | sgn1=signature AT tw=comp_ty DARROW sgn2=signature
    { Sugared.RunnerTy (sgn1, sgn2, tw) }


comp_ty: mark_location(comp_ty_) { $1 }
comp_ty_:
  | t=prod_ty BANG lst=signature
    { Sugared.CompTy (t, lst, None) }

  | t=prod_ty BANG lst=signature AT tw=prod_ty
    { Sugared.CompTy (t, lst, Some tw) }

  | prod_ty_
    { $1 }


prod_ty: mark_location(prod_ty_) { $1 }
prod_ty_:
  | simple_ty_
    { $1 }

  | t=simple_ty STAR ts=separated_nonempty_list(STAR, simple_ty)
    { Sugared.Product (t :: ts) }


simple_ty: mark_location(simple_ty_) { $1 }
simple_ty_:
  | EMPTY
    { Sugared.(Primitive Empty) }

  | INT
    { Sugared.(Primitive Int) }

  | UNIT
    { Sugared.Product [] }

  | BOOL
    { Sugared.(Primitive Bool) }

  | STRING
    { Sugared.(Primitive String) }

  | ANY
    { Sugared.(Primitive Any) }

  | t=var_name
    { Sugared.NamedTy t }

  | LBRACE ops=separated_list(COMMA, var_name) RBRACE
    { Sugared.ContainerTy ops }

  | LPAREN t=ty_ RPAREN
    { t }

datatype:
  | x=var_name EQUAL lst=constructor_clauses
    { (x, lst) }

constructor_clauses:
  | BAR? lst=separated_nonempty_list(BAR, constructor_clause)
    { lst }

constructor_clause:
  | cnstr=CONSTRUCTOR OF t=prod_ty
    { (cnstr, Some t) }

  | cnstr=CONSTRUCTOR
    { (cnstr, None) }

signature:
  | LBRACE lst=separated_list(COMMA, var_name) RBRACE
    { lst }

mark_location(X):
  x=X
  { Location.locate ~loc:(Location.make $startpos $endpos) x }
