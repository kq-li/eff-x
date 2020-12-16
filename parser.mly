%{
  open Ast
%}

%token <int> NUM
%token <string> ID
%token UNIT
%token INT
%token COMMA
%token SEMICOLON
%token COLON
%token BANG
%token READ
%token WRITE
%token OUTPUT
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

%start <Prog.t> prog
%%

prog:
  | funcs = list(func); EOF
    { Core.String.Map.of_alist_exn funcs }

func:
  | name = ID; LPAREN; args = separated_list(COMMA, arg); RPAREN; ret_type = ret_typ?; body = seq
    { let (ret_type, effects) = Base.Option.value ret_type ~default:(Type.Unit, Effect.Set.empty) in
      (name, Func.{ name; args; ret_type; effects; body }) }

ret_typ:
  | COLON; t = eff_typ
    { t }

arg:
  | x = ID; t = typ
    { (x, t) }

seq:
  | LBRACE; ss = list(stmt); RBRACE
    { Stmt.Seq ss }

stmt:
  | x = ID; COLON; t_effs = eff_typ; ASSIGN; e = expr; SEMICOLON
    { let (t, effs) = t_effs in
      Stmt.Assign (x, t, effs, e) }
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
  | LPAREN; RPAREN
    { Value.Unit }
  | n = NUM
    { Value.Int n }
  | x = ID
    { Value.Var x }

expr:
  | v = value
    { Expr.Value v }
  | uop = unop; v = value
    { Expr.Unary (uop, v) }
  | v1 = value; bop = binop; v2 = value
    { Expr.Binary (bop, v1, v2) }
  | f = ID; LPAREN; vs = separated_list(COMMA, value); RPAREN
    { Expr.Apply (f, vs) }

typ:
  | UNIT
    { Type.Unit }
  | INT
    { Type.Int }

eff_typ:
  | t = typ
    { (t, Effect.Set.empty) }
  | t = typ; BANG; effs = separated_nonempty_list(COMMA, effect)
    { (t, Effect.Set.of_list effs) }

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

