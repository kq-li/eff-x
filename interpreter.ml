open! Core
open! Ast

let empty_ctx = String.Map.empty

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
  let bool_of_value ctx v =
    match%bind extract ctx v with
    | Value.Bool b -> Ok b
    | _ -> Or_error.error_s [%message "bool conversion failure" (v : Value.t)]
  in
  let merge ctx ctx' =
    (fun () ->
      Map.merge ctx ctx' ~f:(fun ~key:var -> function
        | `Left v
        | `Right v ->
          Some v
        | `Both (v, v') ->
          if Value.equal v v' then Some v else raise_s [%message "shadowed variable in closure" var]))
    |> Or_error.try_with
  in
  let rec eval_expr ctx = function
    | Expr.Value v ->
      let%map v = extract ctx v in
      (v, empty_ctx)
    | Apply (vf, vs) ->
      ( match (extract_opt ctx vf, vf) with
      | (Some f, _) ->
        let%bind vs = List.map vs ~f:(extract ctx) |> Or_error.combine_errors in
        List.fold_result vs ~init:(f, empty_ctx) ~f:(fun (f, ctx) v ->
            match f with
            | Lambda (x, _, saved_ctx, s) ->
              let%bind ctx = merge ctx saved_ctx in
              let ctx = Map.set ctx ~key:x ~data:v in
              let%map ret_ctx = eval_stmt ctx s in
              let v_ret = lookup_default ret_ctx return_key ~default:Value.Unit in
              (v_ret, ctx)
            | MiniLambda (x, _, saved_ctx, e) ->
              let%bind ctx = merge ctx saved_ctx in
              let ctx = Map.set ctx ~key:x ~data:v in
              let%map (v_ret, _) = eval_expr ctx e in
              (v_ret, ctx)
            | _ ->
              Or_error.error_s
                [%message "invalid function application" (vf : Value.t) (vs : Value.t list)])
      | (None, Var name) when Map.mem Library.funcs name ->
        let f = snd (Map.find_exn Library.funcs name) in
        let%map vs = List.map vs ~f:(extract ctx) |> Or_error.combine_errors in
        (f vs, ctx)
      | _ ->
        Or_error.error_s
          [%message
            "function not found" (vf : Value.t) (vs : Value.t list) (ctx : Value.t String.Map.t)] )
  and eval_stmt ctx s =
    match s with
    | Stmt.Skip -> Ok ctx
    | Assign (x, _, _, e) ->
      let%map v =
        match%bind eval_expr ctx e with
        | (Value.Lambda (x, t, ctx, s), ctx') ->
          let%map ctx = merge ctx ctx' in
          Value.Lambda (x, t, ctx, s)
        | (MiniLambda (x, t, ctx, e), ctx') ->
          let%map ctx = merge ctx ctx' in
          Value.MiniLambda (x, t, ctx, e)
        | (v, _) -> Ok v
      in
      Map.set ctx ~key:x ~data:v
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
  let%map (_ : Value.t String.Map.t) = eval_stmt empty_ctx (Stmt.Seq prog) in
  ()
