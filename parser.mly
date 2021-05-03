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
%token COMMA
%token DOT
%token SEMICOLON
%token COLON
%token ARROW
%token BANG
%token ARR
%token READ
%token WRITE
%token INPUT
%token OUTPUT
%token LAMBDA
%token IF
%token ELSE
%token WHILE
%token FOR
%token RETURN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token LANGLE
%token RANGLE
%token LBRACKET
%token RBRACKET
%token ASSIGN
%token EOF

%right THEN ELSE
%nonassoc TRUE FALSE NUM ID LPAREN
%left APPLY
%nonassoc LBRACKET

%start <Prog.t> prog
%%

prog:
  | body = list(stmt); EOF
    { body }

seq:
  | LBRACE; ss = list(stmt); RBRACE
    { Stmt.Seq ss }

assignable:
  | x = ID
    { Assignable.Var x }
  | a = assignable; LBRACKET; e = expr; RBRACKET
    { Assignable.Sub (a, e) }

stmt:
  | a = assignable; COLON; t = typ; effs = list(eff); ASSIGN; e = assign_expr; SEMICOLON
    { Stmt.Assign (a, t, Effect.Set.of_list effs, e) }
  | IF; LPAREN; e = expr; effs = list(eff); RPAREN; s1 = stmt %prec THEN
    { Stmt.If (e, Effect.Set.of_list effs, s1, Stmt.Skip) }
  | IF; LPAREN; e = expr; effs = list(eff); RPAREN; s1 = stmt; ELSE; s2 = stmt
    { Stmt.If (e, Effect.Set.of_list effs, s1, s2) }
  | WHILE; LPAREN; e = expr; effs = list(eff); RPAREN; s = stmt
    { Stmt.While (e, Effect.Set.of_list effs, s) }
  | FOR; LPAREN; x = ID; COLON; a = NUM; ARROW; b = NUM; RPAREN; s = stmt
    { Stmt.For (x, a, b, s) }
  | RETURN; e = expr; effs = list(eff); SEMICOLON
    { Stmt.Return (e, Effect.Set.of_list effs) }
  | s = seq
    { s }

lambda:
  | LAMBDA; x = ID; COLON; t = typ; DOT; e = expr; effs = list(eff)
    { Value.Lambda (x, t, Core.String.Map.empty, Stmt.Return (e, Effect.Set.of_list effs)) }
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
  | e1 = expr; LBRACKET; e2 = expr; RBRACKET
    { Value.Sub (e1, e2) }
  /* | LBRACKET; es = separated_list(COMMA, expr); RBRACKET */
  /*   { Value.Array es } */
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

assign_expr:
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
  | ARR; LANGLE; t = typ; RANGLE
    { Type.Array (Some t) }
  | LPAREN; t1 = typ; ARROW; t2 = typ; effs = list(eff); RPAREN
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

