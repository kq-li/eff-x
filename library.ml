open! Core
open! Ast

let mem_size = 64
let memory = Array.create ~len:mem_size 0
let no_eff = Effect.Set.empty

let assert_unit = function
  | Value.Unit -> ()
  | _ -> failwith "unit assertion failure"

let assert_int = function
  | Value.Int n -> n
  | _ -> failwith "int assertion failure"

let assert_bool = function
  | Value.Bool b -> b
  | _ -> failwith "bool assertion failure"

let int_int (name, f) = (name, (Type.Fun (Int, Int, no_eff), fun v -> Value.Int (f (assert_int v))))

let bool_bool (name, f) =
  (name, (Type.Fun (Bool, Bool, no_eff), fun v -> Value.Bool (f (assert_bool v))))

let int_int_int (name, f) =
  ( name,
    ( Type.Fun (Int, Type.Fun (Int, Int, no_eff), no_eff),
      fun v1 -> Value.Extern (fun v2 -> Value.Int (f (assert_int v1) (assert_int v2))) ) )

let int_int_bool (name, f) =
  ( name,
    ( Type.Fun (Int, Type.Fun (Int, Bool, no_eff), no_eff),
      fun v1 -> Value.Extern (fun v2 -> Value.Bool (f (assert_int v1) (assert_int v2))) ) )

let bool_bool_bool (name, f) =
  ( name,
    ( Type.Fun (Bool, Type.Fun (Bool, Bool, no_eff), no_eff),
      fun v1 -> Value.Extern (fun v2 -> Value.Bool (f (assert_bool v1) (assert_bool v2))) ) )

[@@@ocamlformat "disable"]

let extern =
  List.concat [
    [
      ("neg", Int.( ~- ));
    ] |> List.map ~f:int_int;
    [
      ("not", not);
    ] |> List.map ~f:bool_bool;
    [
      ("add", Int.( + ));
      ("sub", Int.( - ));
      ("mul", Int.( * ));
      ("div", Int.( / ));
      ("mod", Int.( % ));
    ] |> List.map ~f:int_int_int;
    [
      ("lt", Int.( < ));
      ("le", Int.( <= ));
      ("gt", Int.( > ));
      ("ge", Int.( >= ));
      ("eq", Int.( = ));
      ("neq", Int.( <> ));
    ] |> List.map ~f:int_int_bool;
    [
      ("and", ( && ));
      ("or", ( || ));
    ] |> List.map ~f:bool_bool_bool;
    [
      (
        "scan", (
          Type.Fun (Unit, Int, Effect.Set.singleton Effect.Input),
          fun v -> 
            assert_unit v;
            try
              Out_channel.flush Out_channel.stdout;
              Value.Int (
                In_channel.input_line_exn In_channel.stdin
                |> Int.of_string
              )
            with _ -> failwith "invalid input"
        )
      );
      (
        "print", (
          Type.Fun (Int, Unit, Effect.Set.singleton Effect.Output),
          fun v -> printf "%d\n" (assert_int v); Value.Unit
        )
      );
      (
        "load", (
          Type.Fun (Int, Int, Effect.Set.singleton Effect.Read),
          fun v -> Value.Int memory.(assert_int v)
        )
      );
      (
        "store", (
          Type.Fun (Int, Type.Fun (Int, Unit, Effect.Set.singleton Effect.Write), Effect.Set.empty),
          fun v1 -> Value.Extern (fun v2 -> memory.(assert_int v1) <- assert_int v2; Value.Unit)
        )
      );
    ];
  ] |> String.Map.of_alist_exn

[@@@ocamlformat "enable"]
