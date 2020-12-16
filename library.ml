open! Core
open! Ast

[@@@ocamlformat "disable"]

let funcs =
  [
    (
      "print", (
        None,
        Type.Unit,
        Effect.Set.singleton Effect.Output,
        fun values -> print_s [%message (values : Value.t list)]; Value.Unit
      )
    );
  ]
  |> String.Map.of_alist_exn

[@@@ocamlformat "enable"]
