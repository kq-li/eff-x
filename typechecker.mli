open! Core
open! Ast

module Type_or_index : sig
  type t =
    | Type of Type.t
    | Index
  [@@deriving compare, equal, sexp_of]

  include Comparable.S_plain with type t := t
end

val check_expr : Type_or_index.t String.Map.t -> Expr.t -> (Type.t * Effect.Set.t) Or_error.t

val check_stmt
  :  Type_or_index.t String.Map.t * Effect.Set.t ->
  Stmt.t ->
  (Type_or_index.t String.Map.t * Effect.Set.t) Or_error.t

val check : Prog.t -> unit Or_error.t
