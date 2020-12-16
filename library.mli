open! Core
open! Ast

val funcs : (Type.t list option * Type.t * Effect.Set.t * (Value.t list -> Value.t)) String.Map.t
