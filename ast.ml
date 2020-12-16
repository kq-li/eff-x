open! Core

let return_key = "*retval"

module Op = struct
  module Unary = struct
    type t = Negate [@@deriving compare, equal, sexp_of]
  end

  module Binary = struct
    type t =
      | Plus
      | Minus
      | Multiply
      | Divide
    [@@deriving compare, equal, sexp_of]
  end
end

module Effect = struct
  module T = struct
    type t =
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
    | Fun of t * t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]
end

module rec Value : sig
  type t =
    | Unit
    | Int of int
    | Var of string
    | Lambda of string * Type.t * Stmt.t
  [@@deriving compare, equal, sexp_of]
end = struct
  type t =
    | Unit
    | Int of int
    | Var of string
    | Lambda of string * Type.t * Stmt.t
  [@@deriving compare, equal, sexp_of]
end

and Expr : sig
  type t =
    | Value of Value.t
    | Unary of Op.Unary.t * Value.t
    | Binary of Op.Binary.t * Value.t * Value.t
    | Apply of Value.t * Value.t list
  [@@deriving compare, equal, sexp_of]
end = struct
  type t =
    | Value of Value.t
    | Unary of Op.Unary.t * Value.t
    | Binary of Op.Binary.t * Value.t * Value.t
    | Apply of Value.t * Value.t list
  [@@deriving compare, equal, sexp_of]
end

and Stmt : sig
  type t =
    | Skip
    | Assign of string * Type.t * Effect.Set.t * Expr.t
    | RecAssign of string * Type.t * Effect.Set.t * Expr.t
    | If of Value.t * t * t
    | While of Value.t * t
    | Seq of t list
    | Return of Value.t
  [@@deriving compare, equal, sexp_of]
end = struct
  type t =
    | Skip
    | Assign of string * Type.t * Effect.Set.t * Expr.t
    | RecAssign of string * Type.t * Effect.Set.t * Expr.t
    | If of Value.t * t * t
    | While of Value.t * t
    | Seq of t list
    | Return of Value.t
  [@@deriving compare, equal, sexp_of]
end

module Prog = struct
  type t = Stmt.t list [@@deriving compare, equal, sexp_of]
end
