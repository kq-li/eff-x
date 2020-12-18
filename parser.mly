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
%token OUTPUT
%token LAMBDA
%token IF
%token ELSE
%token WHILE
%token RETURN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token ASSIGN
%token EOF

%right THEN ELSE
%right ARROW

%start <Prog.t> prog
%%

prog:
  | body = list(stmt); EOF
    { body }

seq:
  | LBRACE; ss = list(stmt); RBRACE
    { Stmt.Seq ss }

stmt:
  | x = ID; COLON; t = typ; effs = list(eff); ASSIGN; e = expr; SEMICOLON
    { Stmt.Assign (x, t, Effect.Set.of_list effs, e) }
  | IF; LPAREN; v = value; RPAREN; s1 = stmt %prec THEN
    { Stmt.If (v, s1, Stmt.Skip) }
  | IF; LPAREN; v = value; RPAREN; s1 = stmt; ELSE; s2 = stmt
    { Stmt.If (v, s1, s2) }
  | WHILE; LPAREN; v = value; RPAREN; s = stmt
    { Stmt.While (v, s) }
  | RETURN; v = value; SEMICOLON
    { Stmt.Return v }
  | s = seq
    { s }

value:
  | v = arg_value
    { v }
  | LAMBDA; x = ID; COLON; t = typ; DOT; e = expr
    { Value.MiniLambda (x, t, Core.String.Map.empty, e) }
  | LAMBDA; x = ID; COLON; t = typ; DOT; s = seq
    { Value.Lambda (x, t, Core.String.Map.empty, s) }

arg_value:
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
  | LPAREN; v = value; RPAREN
    { v }

expr:
  | v = value 
    { Expr.Value v }
  | vf = arg_value; vs = arg_value+
    { Expr.Apply (vf, vs) }

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
  | OUTPUT { Effect.Output }
  | READ   { Effect.Read }
  | WRITE  { Effect.Write }

