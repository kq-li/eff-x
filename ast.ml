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

let is_value = function
  | Value _ -> true
  | _ -> false

let value_exn = function
  | Value v -> v
  | expr -> raise_s [%message "not a value" (expr : t)]

let rec free_vars = function
  | Value _ -> String.Set.empty
  | Var x -> String.Set.singleton x
  | Lambda (x, e) -> Set.remove (free_vars e) x
  | Binary (_, e1, e2)
  | Apply (e1, e2)
  | Seq (e1, e2) ->
    Set.union (free_vars e1) (free_vars e2)
