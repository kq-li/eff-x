open! Core
open! Ast

val eval_expr
  :  ?lookup_opt:(Value.t String.Map.t -> String.t -> Value.t option) ->
  Value.t String.Map.t ->
  Expr.t ->
  Value.t Or_error.t

val eval_stmt
  :  ?lookup_opt:(Value.t String.Map.t -> String.t -> Value.t option) ->
  Value.t String.Map.t ->
  Stmt.t ->
  Value.t String.Map.t Or_error.t

val eval
  :  ?lookup_opt:(Value.t String.Map.t -> String.t -> Value.t option) ->
  Prog.t ->
  unit Or_error.t
