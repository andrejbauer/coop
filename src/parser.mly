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
%token COLON ARROW SEMISEMI

(* Expressions and computations *)
%token <int> NUMERAL
%token FUN
%token LET EQUAL IN

(* Toplevel commands *)

%token <string> QUOTED_STRING
%token LOAD
%token OPERATION

(* End of input token *)
%token EOF

(* Precedence and fixity of infix operators *)
%right    ARROW
%left     INFIXOP0
%right    INFIXOP1
%left     INFIXOP2
%left     INFIXOP3
%right    INFIXOP4

%start <Input.toplevel list> file
%start <Input.toplevel> commandline

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
  | c=term             { Input.TopComp c }

(* Things that can be done at the toplevel, except for a computation. *)
toplevel: mark_location(plain_toplevel) { $1 }
plain_toplevel:
  | LOAD fn=QUOTED_STRING                                { Input.TopLoad fn }
  | LET x=var_name EQUAL e=term                          { Input.TopLet (x, e) }
  | OPERATION op=var_name COLON t1=simple_ty ARROW t2=ty { Input.DeclOperation (op, t1, t2) }

(* Main syntax tree *)
term : mark_location(plain_term) { $1 }
plain_term:
  | e=plain_infix_term                            { e }
  | FUN a=lambda_abstraction ARROW e=term         { Input.Lambda (a, e) }
  | LET x=var_name EQUAL c1=infix_term IN c2=term { Input.Let (x, c1, c2) }
  | e=infix_term COLON t=ty                       { Input.Ascribe (e, t) }

infix_term: mark_location(plain_infix_term) { $1 }
plain_infix_term:
  | e=plain_app_term { e }
  | e2=infix_term oploc=infix e3=infix_term
    { let {Location.data=op; loc} = oploc in
      let op = Location.locate ~loc (Input.Var op) in
      let e1 = Location.locate ~loc (Input.Apply (op, e2)) in
      Input.Apply (e1, e3)
    }

app_term: mark_location(plain_app_term) { $1 }
plain_app_term:
  | e=plain_prefix_term          { e }
  | e1=app_term e2=prefix_term   { Input.Apply (e1, e2) }

prefix_term: mark_location(plain_prefix_term) { $1 }
plain_prefix_term:
  | e=plain_simple_term                       { e }
  | oploc=prefix e2=prefix_term
    { let {Location.data=op; loc} = oploc in
      let op = Location.locate ~loc (Input.Var op) in
      Input.Apply (op, e2)
    }

(* simple_term : mark_location(plain_simple_term) { $1 } *)
plain_simple_term:
  | LPAREN e=plain_term RPAREN         { e }
  | n=NUMERAL                          { Input.Numeral n }
  | x=var_name                         { Input.Var x }

var_name:
  | NAME                     { $1 }
  | LPAREN op=infix RPAREN   { op.Location.data }
  | LPAREN op=prefix RPAREN  { op.Location.data }
  | UNDERSCORE               { Name.anonymous () }

%inline infix:
  | op=INFIXOP0    { op }
  | op=INFIXOP1    { op }
  | op=INFIXOP2    { op }
  | op=INFIXOP3    { op }
  | op=INFIXOP4    { op }

%inline prefix:
  | op=PREFIXOP { op }

lambda_abstraction:
  | xs=nonempty_list(var_name)               { [(xs, None)] }
  | lst=nonempty_list(typed_binder)          { List.map (fun (xs, t) -> (xs, Some t)) lst }

typed_binder:
  | LPAREN xs=nonempty_list(var_name) COLON t=ty RPAREN { (xs, t) }

ty:
  | t=simple_ty         { t }
  | t1=ty ARROW t2=ty   { Input.Arrow (t1, t2) }

simple_ty:
  | LPAREN t=ty RPAREN  { t }
  | INT                 { Input.Int }


mark_location(X):
  x=X
  { Location.locate ~loc:(Location.make $startpos $endpos) x }
%%
