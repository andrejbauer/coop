%{
%}

(* Infix operations a la OCaml *)
%token <Name.t Location.located> PREFIXOP INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4 INFIXOP5 STAR BANG AT

(* Names *)
%token <Name.t> NAME
%token UNDERSCORE

(* Primitive types *)
%token INT UNIT

(* Parentheses & punctuations *)
%token LPAREN RPAREN
%token LBRACE RBRACE
%token COLON ARROW DARROW SEMI SEMISEMI COMMA

(* Expressions and computations *)
%token <int> NUMERAL
%token FUN
%token COMODEL
%token LET EQUAL IN
%token MATCH WITH BAR END
%token USING FINALLY VAL

(* Toplevel commands *)

%token <string> QUOTED_STRING
%token LOAD
%token OPERATION
%token EXTERNAL

(* End of input token *)
%token EOF

(* Precedence and fixity of infix operators *)
%nonassoc INFIXOP0
%left     INFIXOP1
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
  | t=toplevel EOF
    { t }

  | t=topcomp EOF
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

  | LET p=pattern EQUAL e=term
    { Sugared.TopLet (p, e) }

  | LET f=var_name a=binding+ EQUAL e=term
    { Sugared.TopLetFun (f, a, e) }

  | OPERATION op=var_name COLON t1=prod_ty ARROW t2=ty
    { Sugared.DeclOperation (op, t1, t2) }

  | EXTERNAL x=var_name COLON t=ty EQUAL s=QUOTED_STRING
    { Sugared.External (x, t, s) }

(* Main syntax tree *)
term : mark_location(term_) { $1 }
term_:
  | e=infix_term_
    { e }

  | e=infix_term COLON t=ty
    { Sugared.Ascribe (e, t) }

  | FUN a=binding+ ARROW e=term
    { Sugared.Lambda (a, e) }

  | LET p=pattern EQUAL c1=infix_term IN c2=term
    { Sugared.Let (p, c1, c2) }

  | LET f=var_name a=binding+ EQUAL c1=infix_term IN c2=term
    { Sugared.LetFun (f, a, c1, c2) }

  | e1=infix_term SEMI e2=term
    { Sugared.Sequence (e1, e2) }

  | MATCH e=infix_term WITH lst=match_clauses END
    { Sugared.Match (e, lst) }

  | COMODEL e=infix_term WITH lst=comodel_clauses END
    { Sugared.Comodel (e, lst) }

  | USING cmdl=infix_term IN c=term FINALLY fin=finally END
    { Sugared.Using (cmdl, c, fin) }

infix_term: mark_location(infix_term_) { $1 }
infix_term_:
  | e=app_term_
    { e }

  | e2=infix_term oploc=infix e3=infix_term
    { let {Location.data=op; loc} = oploc in
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

prefix_term: mark_location(prefix_term_) { $1 }
prefix_term_:
  | e=simple_term_
    { e }

  | oploc=prefix e2=prefix_term
    { let {Location.data=op; loc} = oploc in
      let op = Location.locate ~loc (Sugared.Var op) in
      Sugared.Apply (op, e2)
    }

(* simple_term : mark_location(simple_term_) { $1 } *)
simple_term_:
  | n=NUMERAL
    { Sugared.Numeral n }

  | x=var_name
    { Sugared.Var x }

  | LPAREN es=separated_list(COMMA, term) RPAREN
                { match es with
                  | [e] -> e.Location.data
                  | [] | _::_ -> Sugared.Tuple es }

var_name:
  | NAME                     { $1 }
  | LPAREN op=infix RPAREN   { op.Location.data }
  | LPAREN op=prefix RPAREN  { op.Location.data }

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
  | BAR? lst=separated_list(BAR, match_clause)
    { lst }

match_clause:
  | p=pattern ARROW e=term
    { (p, e) }

comodel_clauses:
  | BAR? lst=separated_list(BAR, comodel_clause)
    { lst }

comodel_clause:
  | op=var_name px=pattern AT pw=pattern ARROW e=term
    { (op, px, pw, e) }

binding:
  | p=pattern
    { (p, None) }

  | LPAREN p=pattern COLON t=ty RPAREN
    { (p, Some t) }

finally:
  | VAL px=pattern AT pw=pattern ARROW t=term
    { (px, pw, t) }


pattern : mark_location(pattern_) { $1 }
pattern_:
  | UNDERSCORE
    { Sugared.PattAnonymous }

  | x=var_name
    { Sugared.PattVar x }

  | k=NUMERAL
    { Sugared.PattNumeral k }

  | LPAREN lst=separated_list(COMMA, pattern) RPAREN
                { match lst with
                  | [p] -> p.Location.data
                  | [] | _::_ -> Sugared.PattTuple lst }


ty: mark_location(ty_) { $1 }
ty_:
  | t=comp_ty_
    { t }

  | t1=comp_ty ARROW t2=ty
    { Sugared.Arrow (t1, t2) }

  | sgn1=signature AT tw=comp_ty DARROW sgn2=signature
    { Sugared.ComodelTy (sgn1, tw, sgn2) }


comp_ty: mark_location(comp_ty_) { $1 }
comp_ty_:
  | t=prod_ty BANG lst=signature
    { Sugared.CompTy (t, lst) }

  | prod_ty_
    { $1 }


prod_ty: mark_location(prod_ty_) { $1 }
prod_ty_:
  | t=simple_ty_
    { t }

  | t=simple_ty STAR ts=separated_nonempty_list(STAR, simple_ty)
    { Sugared.Product (t :: ts) }


simple_ty: mark_location(simple_ty_) { $1 }
simple_ty_:
  | INT
    { Sugared.Int }

  | UNIT
    { Sugared.Product [] }

  | LPAREN t=ty_ RPAREN
    { t }


signature:
  | LBRACE lst=separated_list(COMMA, var_name) RBRACE
    { lst }

mark_location(X):
  x=X
  { Location.locate ~loc:(Location.make $startpos $endpos) x }
%%
