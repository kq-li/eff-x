%{
  open Ast
%}

%token <int> NUM
%token <string> ID
%token TRUE
%token FALSE
%token UNIT
%token INT
%token BOOL
%token DOT
%token SEMICOLON
%token COLON
%token ARROW
%token BANG
%token READ
%token WRITE
%token INPUT
%token OUTPUT
%token LAMBDA
%token IF
%token ELSE
%token WHILE
%token FOR
%token CFOR
%token RETURN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token ASSIGN
%token EOF

%right THEN ELSE
%right ARROW
%nonassoc TRUE FALSE NUM ID LPAREN
%left APPLY

%start <Prog.t> prog
%%

prog:
  | body = list(stmt); EOF
    { body }

seq:
  | LBRACE; ss = list(stmt); RBRACE
    { Stmt.Seq ss }

stmt:
  | x = ID; COLON; t = typ; effs = list(eff); ASSIGN; e = toplevel_expr; SEMICOLON
    { Stmt.Assign (x, t, Effect.Set.of_list effs, e) }
  | IF; LPAREN; e = toplevel_expr; RPAREN; s1 = stmt %prec THEN
    { Stmt.If (e, s1, Stmt.Skip) }
  | IF; LPAREN; e = toplevel_expr; RPAREN; s1 = stmt; ELSE; s2 = stmt
    { Stmt.If (e, s1, s2) }
  | WHILE; LPAREN; e = toplevel_expr; RPAREN; s = stmt
    { Stmt.While (e, s) }
  | FOR; LPAREN; x = ID; COLON; a = NUM; ARROW; b = NUM; RPAREN; s = stmt
    { Stmt.For (x, a, b, s) }
  | CFOR; LPAREN; x = ID; COLON; a = NUM; ARROW; b = NUM; RPAREN; s = stmt
    { Stmt.CFor (x, a, b, s) }
  | RETURN; e = toplevel_expr; SEMICOLON
    { Stmt.Return e }
  | s = seq
    { s }

lambda:
  | LAMBDA; x = ID; COLON; t = typ; DOT; e = toplevel_expr
    { Value.Lambda (x, t, Core.String.Map.empty, Stmt.Return e) }
  | LAMBDA; x = ID; COLON; t = typ; DOT; s = seq
    { Value.Lambda (x, t, Core.String.Map.empty, s) }

value:
  | LPAREN; RPAREN
    { Value.Unit }
  | n = NUM
    { Value.Int n }
  | TRUE
    { Value.Bool true }
  | FALSE
    { Value.Bool false }
  | x = ID
    { Value.Var x }
  | LPAREN; v = lambda; RPAREN
    { v }

expr:
  | v = value 
    { Expr.Value v }
  | LPAREN; e = expr; RPAREN
    { e }
  | e1 = expr; e2 = expr %prec APPLY
    { Expr.Apply (e1, e2) }

toplevel_expr:
  | e = expr
    { e }
  | v = lambda
    { Expr.Value v }

typ:
  | UNIT
    { Type.Unit }
  | INT
    { Type.Int }
  | BOOL
    { Type.Bool }
  | t1 = typ; ARROW; t2 = typ
    { Type.Fun (t1, t2, Effect.Set.empty) }
  | LPAREN; t1 = typ; ARROW; t2 = typ; effs = nonempty_list(eff); RPAREN
    { Type.Fun (t1, t2, Effect.Set.of_list effs) }
  | LPAREN; t = typ; RPAREN
    { t }
    
eff:
  | BANG; e = effect
    { e }

%inline effect:
  | INPUT  { Effect.Input }
  | OUTPUT { Effect.Output }
  | READ   { Effect.Read }
  | WRITE  { Effect.Write }

