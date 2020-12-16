open! Core
open! Ast

[@@@ocamlformat "disable"]

let funcs =
  [
    (
      "print", (
        Type.Fun (Int, Unit, Effect.Set.singleton Effect.Output),
        function
        | [Value.Int n] -> printf "%d\n" n; Value.Unit
        | values ->
          print_s [%message (values : Value.t list)]; Value.Unit
      )
    );
  ]
  |> String.Map.of_alist_exn

[@@@ocamlformat "enable"]
