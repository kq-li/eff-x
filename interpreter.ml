open! Core
open! Ast

let eval prog =
  let open Or_error.Let_syntax in
  let lookup_opt ctx x = Map.find ctx x in
  let lookup ctx x =
    match lookup_opt ctx x with
    | None -> Or_error.error_s [%message "variable not found" x (ctx : Value.t String.Map.t)]
    | Some v -> Ok v
  in
  let lookup_default ctx x ~default = lookup_opt ctx x |> Option.value ~default in
  let extract ctx = function
    | Value.Var x -> lookup ctx x
    | value -> Ok value
  in
  let extract_opt ctx x =
    match extract ctx x with
    | Ok v -> Some v
    | _ -> None
  in
  let rec int_of_value ctx = function
    | Value.Unit -> Or_error.error_s [%message "cannot convert unit to int"]
    | Int n -> Ok n
    | value ->
      let%bind v = extract ctx value in
      int_of_value ctx v
  in
  let bool_of_value ctx v =
    let%map n = int_of_value ctx v in
    n <> 0
  in
  let rec eval_expr ctx = function
    | Expr.Value v -> extract ctx v
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
    | Apply (vf, vs) ->
      ( match (extract_opt ctx vf, vf) with
      | (Some f, _) ->
        let%bind vs = List.map vs ~f:(extract ctx) |> Or_error.combine_errors in
        List.fold_result vs ~init:(ctx, f) ~f:(fun (ctx, f) v ->
            match f with
            | Lambda (x, _, s) ->
              let ctx = Map.set ctx ~key:x ~data:v in
              let%map ctx = eval_stmt ctx s in
              let v_ret = lookup_default ctx return_key ~default:Value.Unit in
              (ctx, v_ret)
            | _ ->
              Or_error.error_s
                [%message "invalid function application" (vf : Value.t) (vs : Value.t list)])
        |> Or_error.map ~f:snd
      | (None, Var name) when Map.mem Library.funcs name ->
        let f = snd (Map.find_exn Library.funcs name) in
        let%map vs = List.map vs ~f:(extract ctx) |> Or_error.combine_errors in
        f vs
      | _ -> Or_error.error_s [%message "function not found" (vf : Value.t) (vs : Value.t list)] )
  and eval_stmt ctx = function
    | Stmt.Skip -> Ok ctx
    | RecAssign (x, _, _, Value (Var y)) when String.equal x y ->
      Or_error.error_s [%message "invalid recursion"]
    | RecAssign (x, _, _, e)
    | Assign (x, _, _, e) ->
      let%bind v = eval_expr ctx e in
      Ok (Map.set ctx ~key:x ~data:v)
    | If (v, s1, s2) -> if%bind bool_of_value ctx v then eval_stmt ctx s1 else eval_stmt ctx s2
    | While (v, s) ->
      if%bind bool_of_value ctx v then
        let%bind ctx = eval_stmt ctx s in
        eval_stmt ctx (While (v, s))
      else Ok ctx
    | Seq ss ->
      let%map (_, ctx) =
        List.fold_result ss ~init:(true, ctx) ~f:(fun (continue, ctx) stmt ->
            let continue =
              match stmt with
              | Stmt.Return _ -> false
              | _ -> continue
            in
            let%map ctx = eval_stmt ctx stmt in
            (continue, ctx))
      in
      ctx
    | Return v ->
      let%bind v = extract ctx v in
      Ok (Map.set ctx ~key:return_key ~data:v)
  in
  let%map (_ : Value.t String.Map.t) = eval_stmt String.Map.empty (Stmt.Seq prog) in
  ()
