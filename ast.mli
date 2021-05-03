open! Core

val return_key : string

module Effect : sig
  type t =
    | Input
    | Output
    | Read
    | Write
  [@@deriving compare, equal, sexp_of]

  include Comparable.S_plain with type t := t

  val noncommutative : Set.t
  val to_json : t -> Yojson.Basic.t
end

module Type : sig
  type t =
    | Unit
    | Int
    | Bool
    | Array of t option
    | Fun of t * t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]

  val to_json : t -> Yojson.Basic.t
end

module rec Value : sig
  type t =
    | Unit
    | Int of int
    | Bool of bool
    | Sub of Expr.t * Expr.t
    | Array of Expr.t list
    | Var of string
    | Lambda of string * Type.t * Value.t String.Map.t * Stmt.t
  [@@deriving compare, equal, sexp_of]

  val used_vars : t -> String.Set.t
  val to_json : t -> Yojson.Basic.t
end

and Expr : sig
  type t =
    | Value of Value.t
    | Apply of t * t
  [@@deriving compare, equal, sexp_of]

  val used_vars : t -> String.Set.t
  val to_json : t -> Yojson.Basic.t
end

and Assignable : sig
  type t =
    | Var of string
    | Sub of t * Expr.t
  [@@deriving compare, equal, sexp_of]

  val used_vars : t -> String.Set.t
  val to_json : t -> Yojson.Basic.t
end

and Stmt : sig
  type t =
    | Skip
    | Assign of Assignable.t * Type.t * Effect.Set.t * Expr.t
    | If of Expr.t * Effect.Set.t * t * t
    | While of Expr.t * Effect.Set.t * t
    | For of string * Expr.t * Expr.t * Expr.t * Effect.Set.t * t
    (* variable, start, end, step, effs, (accumulator, reducer) list, body *)
    | CFor of string * Expr.t * Expr.t * Expr.t * Effect.Set.t * (string * string) list * t
    | Seq of t list
    | Return of Expr.t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]

  val all_effs : t -> Effect.Set.t
  val used_vars : t -> String.Set.t
  val to_json : t -> Yojson.Basic.t
end

module Prog : sig
  type t = Stmt.t list [@@deriving compare, equal, sexp_of]

  val to_json : t -> Yojson.Basic.t
end
