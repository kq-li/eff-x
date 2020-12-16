open! Core
open! Ast

let eval prog =
  let open Or_error.Let_syntax in
  let extract_value ctx = function
    | Value.Var x -> Map.find_or_error ctx x
    | value -> Ok value
  in
  let rec int_of_value ctx = function
    | Value.Unit -> Or_error.error_s [%message "cannot convert unit to int"]
    | Int n -> Ok n
    | value ->
      let%bind v = extract_value ctx value in
      int_of_value ctx v
  in
  let bool_of_value ctx v =
    let%map n = int_of_value ctx v in
    n <> 0
  in
  let rec eval_expr ctx = function
    | Expr.Value v -> extract_value ctx v
    | Unary (Op.Unary.Negate, v) ->
      let%map n = int_of_value ctx v in
      Value.Int (-n)
    | Binary (binop, v1, v2) ->
      let%bind n1 = int_of_value ctx v1 in
      let%map n2 = int_of_value ctx v2 in
      let f =
        match binop with
        | Op.Binary.Plus -> Int.( + )
        | Minus -> Int.( - )
        | Multiply -> Int.( * )
        | Divide -> Int.( / )
      in
      Value.Int (f n1 n2)
    | Apply (f, vs) ->
      let%bind args = List.map vs ~f:(extract_value ctx) |> Or_error.combine_errors in
      eval_func f args
  and eval_stmt ctx = function
    | Stmt.Skip -> Ok ctx
    | Assign (x, _, _, e) ->
      let%bind v = eval_expr ctx e in
      Ok (Map.set ctx ~key:x ~data:v)
    | If (v, s1, s2) -> if%bind bool_of_value ctx v then eval_stmt ctx s1 else eval_stmt ctx s2
    | While (v, s) ->
      if%bind bool_of_value ctx v then
        let%bind ctx = eval_stmt ctx s in
        eval_stmt ctx (While (v, s))
      else Ok ctx
    | Seq ss -> List.fold_result ss ~init:ctx ~f:eval_stmt
    | Return v ->
      let%bind v = extract_value ctx v in
      Ok (Map.set ctx ~key:return_key ~data:v)
  and eval_func name values =
    match (Map.find prog name, Map.find Library.funcs name) with
    | (None, None) -> Or_error.error_s [%message "function not found" name]
    | (Some Func.{ args; body; _ }, _) ->
      let ctx =
        match List.zip (List.map args ~f:fst) values with
        | Unequal_lengths ->
          raise_s
            [%message
              "function called with wrong number of arguments"
                name
                (args : (string * Type.t) list)
                (values : Value.t list)]
        | Ok arg_exprs -> String.Map.of_alist_exn arg_exprs
      in
      let%map ctx = eval_stmt ctx body in
      Map.find ctx return_key |> Option.value ~default:Value.Unit
    | (None, Some (_, _, _, f)) -> Ok (f values)
  in
  match eval_func "main" [] |> ok_exn with
  | Value.Int code -> code
  | value -> raise_s [%message "got invalid return value" (value : Value.t)]
