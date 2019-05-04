%{
%}

(* Infix operations a la OCaml *)
%token <Name.ident Location.located> PREFIXOP INFIXOP0 INFIXOP1 INFIXOP2 INFIXOP3 INFIXOP4

(* Names *)
%token <Name.ident> NAME
%token UNDERSCORE

(* Primitive types *)
%token INT

(* Parentheses & punctuations *)
%token LPAREN RPAREN
%token COLON ARROW SEMISEMI COMMA STAR

(* Expressions and computations *)
%token <int> NUMERAL
%token FUN
%token LET EQUAL IN
%token MATCH WITH BAR END

(* Toplevel commands *)

%token <string> QUOTED_STRING
%token LOAD
%token OPERATION

(* End of input token *)
%token EOF

(* Precedence and fixity of infix operators *)
%left     INFIXOP0
%right    INFIXOP1
%left     INFIXOP2
%left     INFIXOP3
%right    INFIXOP4

%start <Sugared.toplevel list> file
%start <Sugared.toplevel> commandline

%%

(* Toplevel syntax *)

file:
  | f=filecontents EOF            { f }

filecontents:
  |                                         { [] }
  | d=topcomp                               { [d] }
  | d=toplevel SEMISEMI ds=filecontents     { d :: ds }
  | d=topcomp  SEMISEMI ds=filecontents     { d :: ds }
  | d=toplevel ds=filecontents_top          { d :: ds }

filecontents_top:
  |                                         { [] }
  | d=toplevel SEMISEMI ds=filecontents     { d :: ds }
  | d=toplevel ds=filecontents_top          { d :: ds }

commandline:
  | t=toplevel EOF     { t }
  | t=topcomp EOF      { t }

(* Toplevel computation *)
topcomp: mark_location(plain_topcomp) { $1 }
plain_topcomp:
  | c=term             { Sugared.TopComp c }

(* Things that can be done at the toplevel, except for a computation. *)
toplevel: mark_location(plain_toplevel) { $1 }
plain_toplevel:
  | LOAD fn=QUOTED_STRING                                { Sugared.TopLoad fn }
  | LET p=pattern EQUAL e=term                           { Sugared.TopLet (p, e) }
  | OPERATION op=var_name COLON t1=simple_ty ARROW t2=ty { Sugared.DeclOperation (op, t1, t2) }

(* Main syntax tree *)
term : mark_location(plain_term) { $1 }
plain_term:
  | e=plain_infix_term                            { e }
  | FUN a=lambda_abstraction ARROW e=term         { Sugared.Lambda (a, e) }
  | LET p=pattern EQUAL c1=infix_term IN c2=term  { Sugared.Let (p, c1, c2) }
  | MATCH e=infix_term WITH lst=match_clauses END { Sugared.Match (e, lst) }
  | e=infix_term COLON t=ty                       { Sugared.Ascribe (e, t) }

infix_term: mark_location(plain_infix_term) { $1 }
plain_infix_term:
  | e=plain_app_term { e }
  | e2=infix_term oploc=infix e3=infix_term
    { let {Location.data=op; loc} = oploc in
      let op = Location.locate ~loc (Sugared.Var op) in
      let e1 = Location.locate ~loc (Sugared.Apply (op, e2)) in
      Sugared.Apply (e1, e3)
    }

app_term: mark_location(plain_app_term) { $1 }
plain_app_term:
  | e=plain_prefix_term          { e }
  | e1=app_term e2=prefix_term   { Sugared.Apply (e1, e2) }

prefix_term: mark_location(plain_prefix_term) { $1 }
plain_prefix_term:
  | e=plain_simple_term                       { e }
  | oploc=prefix e2=prefix_term
    { let {Location.data=op; loc} = oploc in
      let op = Location.locate ~loc (Sugared.Var op) in
      Sugared.Apply (op, e2)
    }

(* simple_term : mark_location(plain_simple_term) { $1 } *)
plain_simple_term:
  | n=NUMERAL   { Sugared.Numeral n }
  | x=var_name  { Sugared.Var x }
  | LPAREN es=separated_nonempty_list(COMMA, term) RPAREN
                { match es with
                  | [] -> assert false
                  | [e] -> e.Location.data
                  | _::_ -> Sugared.Tuple es }

var_name:
  | NAME                     { $1 }
  | LPAREN op=infix RPAREN   { op.Location.data }
  | LPAREN op=prefix RPAREN  { op.Location.data }

%inline infix:
  | op=INFIXOP0    { op }
  | op=INFIXOP1    { op }
  | op=INFIXOP2    { op }
  | op=INFIXOP3    { op }
  | op=INFIXOP4    { op }

%inline prefix:
  | op=PREFIXOP { op }

match_clauses:
  | BAR? lst=separated_list(BAR, match_clause)  { lst }

match_clause:
  | p=pattern ARROW e=term   { (p, e) }

lambda_abstraction:
  | xs=nonempty_list(var_name)               { [(xs, None)] }
  | lst=nonempty_list(typed_binder)          { List.map (fun (xs, t) -> (xs, Some t)) lst }

typed_binder:
  | LPAREN xs=nonempty_list(var_name) COLON t=ty RPAREN { (xs, t) }

pattern : mark_location(plain_pattern) { $1 }
plain_pattern:
  | UNDERSCORE  { Sugared.PattAnonymous }
  | x=var_name  { Sugared.PattVar x }
  | k=NUMERAL   { Sugared.PattNumeral k }
  | LPAREN lst=separated_nonempty_list(COMMA, pattern) RPAREN
                { match lst with
                  | [] -> assert false
                  | [p] -> p.Location.data
                  | _::_ -> Sugared.PattTuple lst }

ty:
  | t=prod_ty               { t }
  | t1=prod_ty ARROW t2=ty  { Sugared.Arrow (t1, t2) }

prod_ty:
  | t=simple_ty   { t }
  | t=simple_ty STAR ts=separated_nonempty_list(STAR, simple_ty)
                  { Sugared.Product (t :: ts) }

simple_ty:
  | LPAREN t=ty RPAREN  { t }
  | INT                 { Sugared.Int }


mark_location(X):
  x=X
  { Location.locate ~loc:(Location.make $startpos $endpos) x }
%%
