%{
  open Ast
%}

%token <int> NUM
%token <string> ID
%token INT
%token ARROW
%token LAMBDA
%token DOT
%token COLON
%token SEMICOLON
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE
%token EOF

%right FUNC
%right SEMICOLON ARROW
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NUM ID LPAREN LAMBDA

%start <Expr.t option> expr_opt
%%

expr_opt:
  | EOF
    { None }
  | e = expr; EOF
    { Some e }

expr:
  | n = NUM
    { Expr.Value n }
  | x = ID
    { Expr.Var x }
  | LPAREN; e = expr; RPAREN
    { e }
  | e1 = expr; bop = binop; e2 = expr
    { Expr.Binary (bop, e1, e2) }
  | LAMBDA; x = ID; COLON; t = typ; DOT; e = expr %prec FUNC
    { Expr.Lambda (x, t, e) }
  | e1 = expr; e2 = expr %prec NUM
    { Expr.Apply (e1, e2) }
  | e1 = expr; SEMICOLON; e2 = expr
    { Expr.Seq (e1, e2) }

typ:
  | INT
    { Type.Int }
  | t1 = typ; ARROW; t2 = typ
    { Type.Fun (t1, t2) }

%inline binop:
  | PLUS     { Plus }
  | MINUS    { Minus }
  | MULTIPLY { Multiply }
  | DIVIDE   { Divide }
