open! Core

module Op : sig
  type t =
    | Plus
    | Minus
    | Multiply
    | Divide
  [@@deriving compare, equal, sexp_of]
end

module Effect : sig
  type t = Output [@@deriving compare, equal, sexp_of]

  include Comparable.S_plain with type t := t
end

module Type : sig
  type t =
    | Int
    | Fun of t * t
    | With_effect of t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]
end

module Expr : sig
  type t =
    | Value of int
    | Var of string
    | Binary of Op.t * t * t
    | Lambda of string * Type.t * t
    | Apply of t * t
    | Seq of t * t
  [@@deriving compare, equal, sexp_of]

  val is_value : t -> bool
  val value_exn : t -> int
  val free_vars : t -> String.Set.t
  val check_type : t -> Type.t Or_error.t
end
