open! Core
open! Ast

let mem_size = 64
let memory = Array.create ~len:mem_size 0
let no_eff = Effect.Set.empty

let int_int (name, f) =
  ( name,
    ( Type.Fun (Int, Int, no_eff),
      function
      | [ Value.Int n ] -> Value.Int (f n)
      | _ -> failwith "impossible" ) )

let bool_bool (name, f) =
  ( name,
    ( Type.Fun (Bool, Bool, no_eff),
      function
      | [ Value.Bool b ] -> Value.Bool (f b)
      | _ -> failwith "impossible" ) )

let defer name t_lambda v =
  let temp1 = "*temp1" in
  let temp2 = "*temp2" in
  Value.MiniLambda
    (temp2, t_lambda, String.Map.singleton temp1 v, Apply (Var name, [ Var temp1; Var temp2 ]))

let int_int_int (name, f) =
  ( name,
    ( Type.Fun (Int, Type.Fun (Int, Int, no_eff), no_eff),
      function
      | [ v ] -> defer name (Type.Fun (Int, Int, no_eff)) v
      | [ Value.Int a; Int b ] -> Value.Int (f a b)
      | _ -> failwith "impossible" ) )

let int_int_bool (name, f) =
  ( name,
    ( Type.Fun (Int, Type.Fun (Int, Bool, no_eff), no_eff),
      function
      | [ v ] -> defer name (Type.Fun (Int, Bool, no_eff)) v
      | [ Value.Int a; Int b ] -> Value.Bool (f a b)
      | _ -> failwith "impossible" ) )

let bool_bool_bool (name, f) =
  ( name,
    ( Type.Fun (Bool, Type.Fun (Bool, Bool, no_eff), no_eff),
      function
      | [ v ] -> defer name (Type.Fun (Bool, Bool, no_eff)) v
      | [ Value.Bool a; Bool b ] -> Value.Bool (f a b)
      | _ -> failwith "impossible" ) )

[@@@ocamlformat "disable"]

let funcs =
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
        "print", (
          Type.Fun (Int, Unit, Effect.Set.singleton Effect.Output),
          function 
          | [Value.Int n] -> printf "%d\n" n; Value.Unit
          | _ -> failwith "impossible"
        )
      );
      (
        "load", (
          Type.Fun (Int, Int, Effect.Set.singleton Effect.Read),
          function
          | [Value.Int n] -> Value.Int memory.(n)
          | _ -> failwith "impossible"
        )
      );
      (
        "store", (
          Type.Fun (Int, Type.Fun (Int, Unit, Effect.Set.singleton Effect.Write), Effect.Set.empty),
          function
          | [Value.Int addr; Value.Int data] -> memory.(addr) <- data; Value.Unit
          | _ -> failwith "impossible"
        )
      );
    ];
  ] |> String.Map.of_alist_exn

[@@@ocamlformat "enable"]
