open! Core
open! Ast

[@@@ocamlformat "disable"]

let vars =
  []
  |> String.Map.of_alist_exn

let funcs =
  [
    ("print", fun expr -> print_s [%message (expr : Expr.t)]);
  ]

[@@@ocamlformat "enable"]
