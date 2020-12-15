open! Core
open! Ast

[@@@ocamlformat "disable"]

let vars =
  []
  |> String.Map.of_alist_exn

let funcs =
  [
    ("print", fun values -> print_s [%message (values : Value.t list)]; Value.Unit);
  ]
  |> String.Map.of_alist_exn

[@@@ocamlformat "enable"]
