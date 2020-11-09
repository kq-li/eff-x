open! Core

val subst : Ast.t -> string -> Ast.t -> Ast.t
val reduce : Ast.t -> Ast.t
val eval : Ast.t -> int Or_error.t
