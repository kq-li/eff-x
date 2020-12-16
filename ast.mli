open! Core

val return_key : string

module Op : sig
  module Unary : sig
    type t = Negate [@@deriving compare, equal, sexp_of]
  end

  module Binary : sig
    type t =
      | Plus
      | Minus
      | Multiply
      | Divide
    [@@deriving compare, equal, sexp_of]
  end
end

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
    | Fun of t list * t
    | With_effect of t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]
end

module Value : sig
  type t =
    | Unit
    | Int of int
    | Var of string
  [@@deriving compare, equal, sexp_of]
end

module Expr : sig
  type t =
    | Value of Value.t
    | Unary of Op.Unary.t * Value.t
    | Binary of Op.Binary.t * Value.t * Value.t
    | Apply of string * Value.t list
  [@@deriving compare, equal, sexp_of]
end

module Stmt : sig
  type t =
    | Skip
    | Assign of string * Type.t * Effect.Set.t * Expr.t
    | If of Value.t * t * t
    | While of Value.t * t
    | Seq of t list
    | Return of Value.t
  [@@deriving compare, equal, sexp_of]
end

module Func : sig
  type t = {
    name : string;
    args : (string * Type.t) list;
    ret_type : Type.t;
    effects : Effect.Set.t;
    body : Stmt.t;
  }
  [@@deriving compare, equal, sexp_of]
end

module Prog : sig
  type t = Func.t String.Map.t [@@deriving compare, equal, sexp_of]
end
