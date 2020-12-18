open! Core
open! Ast

val eval_expr : Value.t String.Map.t -> Expr.t -> Value.t Or_error.t
val eval_stmt : Value.t String.Map.t -> Stmt.t -> Value.t String.Map.t Or_error.t
val eval : ?count:bool -> Prog.t -> unit Or_error.t
