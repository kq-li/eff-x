%{
  open Ast
%}

%token <int> NUM
%token <string> ID
%token UNIT
%token INT
%token DOT
%token SEMICOLON
%token COLON
%token ARROW
%token BANG
%token READ
%token WRITE
%token OUTPUT
%token LAMBDA
%token REC
%token IF
%token ELSE
%token WHILE
%token RETURN
%token LPAREN
%token RPAREN
%token LBRACE
%token RBRACE
%token ASSIGN
%token PLUS
%token MINUS
%token MULTIPLY
%token DIVIDE
%token EOF

%right THEN ELSE
%right ARROW
%right VALUE PLUS MINUS MULTIPLY DIVIDE 

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
  | REC; x = ID; COLON; t = typ; effs = list(eff); ASSIGN; e = expr; SEMICOLON
    { Stmt.RecAssign (x, t, Effect.Set.of_list effs, e) }
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
  | x = ID
    { Value.Var x }
  | LPAREN; v = value; RPAREN
    { v }

expr:
  | v = value %prec VALUE
    { Expr.Value v }
  | uop = unop; v = value
    { Expr.Unary (uop, v) }
  | v1 = value; bop = binop; v2 = value
    { Expr.Binary (bop, v1, v2) }
  | vf = arg_value; vs = arg_value+
    { Expr.Apply (vf, vs) }

typ:
  | UNIT
    { Type.Unit }
  | INT
    { Type.Int }
  | t1 = typ; ARROW; t2 = typ
    { Type.Fun (t1, t2, Effect.Set.empty) }
  | LPAREN; t1 = typ; ARROW; t2 = typ; effs = nonempty_list(eff); RPAREN
    { Type.Fun (t1, t2, Effect.Set.of_list effs) }
  | LPAREN; t = typ; RPAREN
    { t }
    
eff:
  | BANG; e = effect
    { e }

%inline unop:
  | MINUS { Op.Unary.Negate }

%inline binop:
  | PLUS     { Op.Binary.Plus }
  | MINUS    { Op.Binary.Minus }
  | MULTIPLY { Op.Binary.Multiply }
  | DIVIDE   { Op.Binary.Divide }

%inline effect:
  | OUTPUT { Effect.Output }
  | READ   { Effect.Read }
  | WRITE  { Effect.Write }

