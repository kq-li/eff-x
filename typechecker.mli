open! Core
open! Ast

val check_expr : Type.t String.Map.t -> Expr.t -> (Type.t * Effect.Set.t) Or_error.t

val check_stmt
  :  Type.t String.Map.t * Effect.Set.t ->
  Stmt.t ->
  (Type.t String.Map.t * Effect.Set.t) Or_error.t

val check : Prog.t -> unit Or_error.t
