%{
  open Ast
%}

%token <int> INT
%token <string> VAR
%token LAMBDA
%token DOT
%token SEMICOLON
%token LPAREN
%token RPAREN
%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE
%token EOF

%right FUNC
%right SEMICOLON
%left PLUS MINUS
%left MULTIPLY DIVIDE
%left INT VAR LPAREN LAMBDA

%start <t option> expr_opt
%%

expr_opt:
  | EOF
    { None }
  | e = expr; EOF
    { Some e }

expr:
  | n = INT
    { Value n }
  | var = VAR
    { Var var }
  | LPAREN; e = expr; RPAREN
    { e }
  | e1 = expr; bop = binop; e2 = expr
    { Binary (bop, e1, e2) }
  | LAMBDA; arg = VAR; DOT; e = expr %prec FUNC
    { Lambda (arg, e) }
  | e1 = expr; e2 = expr %prec INT
    { Apply (e1, e2) }
  | e1 = expr; SEMICOLON; e2 = expr
    { Seq (e1, e2) }

%inline binop:
  | PLUS     { Plus }
  | MINUS    { Minus }
  | MULTIPLY { Multiply }
  | DIVIDE   { Divide }
