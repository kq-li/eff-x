open! Core
open! Ast

let no_eff = Effect.Set.empty

let int_int name = (name, Type.Fun (Int, Int, no_eff))

let bool_bool name = (name, Type.Fun (Bool, Bool, no_eff))

let int_int_int name = (name, Type.Fun (Int, Type.Fun (Int, Int, no_eff), no_eff))

let int_int_bool name = (name, Type.Fun (Int, Type.Fun (Int, Bool, no_eff), no_eff))

let bool_bool_bool name = (name, Type.Fun (Bool, Type.Fun (Bool, Bool, no_eff), no_eff))

[@@@ocamlformat "disable"]

let extern =
  List.concat [
    ["neg"] |> List.map ~f:int_int;
    ["not"] |> List.map ~f:bool_bool;
    [
      "add";
      "sub";
      "mul";
      "div";
      "mod";
    ] |> List.map ~f:int_int_int;
    [
      "lt";
      "le";
      "gt";
      "ge";
      "eq";
      "neq";
    ] |> List.map ~f:int_int_bool;
    ["and"; "or"] |> List.map ~f:bool_bool_bool;
    [
      ("scan", Type.Fun (Unit, Int, Effect.Set.singleton Effect.Input));
      ("print", Type.Fun (Int, Unit, Effect.Set.singleton Effect.Output));
      ("load", Type.Fun (Int, Int, Effect.Set.singleton Effect.Read));
      ("store", Type.Fun (Int, Type.Fun (Int, Unit, Effect.Set.singleton Effect.Write), Effect.Set.empty));
    ];
  ] |> String.Map.of_alist_exn

[@@@ocamlformat "enable"]
