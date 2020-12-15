open! Core
open! Ast

val vars : Expr.t String.Map.t
val funcs : (Value.t list -> Value.t) String.Map.t
