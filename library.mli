open! Core
open! Ast

val vars : Expr.t String.Map.t
val funcs : (Expr.t -> Expr.t) String.Map.t
