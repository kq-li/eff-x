open! Core

type op =
  | Plus
  | Minus
  | Multiply
  | Divide
[@@deriving compare, equal, sexp_of]

type t =
  | Value of int
  | Var of string
  | Binary of op * t * t
  | Lambda of string * t
  | Apply of t * t
  | Seq of t * t
[@@deriving compare, equal, sexp_of]

val is_value : t -> bool
val value_exn : t -> int
val free_vars : t -> String.Set.t
