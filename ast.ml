open! Core

let return_key = "*retval"

module Effect = struct
  module T = struct
    type t =
      | Input
      | Output
      | Read
      | Write
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

module Type = struct
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
    | Extern of (t -> t)
  [@@deriving compare, equal, sexp_of]
end = struct
  type t =
    | Unit
    | Int of int
    | Bool of bool
    | Var of string
    | Lambda of string * Type.t * Value.t String.Map.t * Stmt.t
    | Extern of ((t -> t)[@compare.ignore] [@equal.ignore])
  [@@deriving compare, equal, sexp_of]
end

and Expr : sig
  type t =
    | Value of Value.t
    | Apply of t * t
  [@@deriving compare, equal, sexp_of]
end = struct
  type t =
    | Value of Value.t
    | Apply of t * t
  [@@deriving compare, equal, sexp_of]
end

and Stmt : sig
  type t =
    | Skip
    | Assign of string * Type.t * Effect.Set.t * Expr.t
    | If of Expr.t * t * t
    | While of Expr.t * t
    | Seq of t list
    | Return of Expr.t
  [@@deriving compare, equal, sexp_of]
end = struct
  type t =
    | Skip
    | Assign of string * Type.t * Effect.Set.t * Expr.t
    | If of Expr.t * t * t
    | While of Expr.t * t
    | Seq of t list
    | Return of Expr.t
  [@@deriving compare, equal, sexp_of]
end

module Prog = struct
  type t = Stmt.t list [@@deriving compare, equal, sexp_of]
end
