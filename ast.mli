open! Core

val return_key : string

module Effect : sig
  type t =
    | Output
    | Read
    | Write
  [@@deriving compare, equal, sexp_of]

  include Comparable.S_plain with type t := t
end

module Type : sig
  type t =
    | Unit
    | Int
    | Bool
    | Fun of t * t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]
end

module rec Value : sig
  type t =
    | Unit
    | Int of int
    | Bool of bool
    | Var of string
    | Lambda of string * Type.t * Value.t String.Map.t * Stmt.t
    | MiniLambda of string * Type.t * Value.t String.Map.t * Expr.t
  [@@deriving compare, equal, sexp_of]
end

and Expr : sig
  type t =
    | Value of Value.t
    | Apply of Value.t * Value.t list
  [@@deriving compare, equal, sexp_of]
end

and Stmt : sig
  type t =
    | Skip
    | Assign of string * Type.t * Effect.Set.t * Expr.t
    | If of Value.t * t * t
    | While of Value.t * t
    | Seq of t list
    | Return of Value.t
  [@@deriving compare, equal, sexp_of]
end

module Prog : sig
  type t = Stmt.t list [@@deriving compare, equal, sexp_of]
end
