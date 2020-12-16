open! Core
open! Ast

let check prog =
  let open Or_error.Let_syntax in
  let lookup ctx x =
    match Map.find ctx x with
    | None -> Or_error.error_s [%message "variable not found in type context" x]
    | Some v -> Ok v
  in
  let lookup_default ctx x ~default = Map.find ctx x |> Option.value ~default in
  let rec check_value ctx = function
    | Value.Unit -> Ok Type.Unit
    | Int _ -> Ok Type.Int
    | Var x -> lookup ctx x
    | MiniLambda (x, t, _, e) ->
      let%map (t_ret, effs) = check_expr (Map.set ctx ~key:x ~data:t) e in
      Type.Fun (t, t_ret, effs)
    | Lambda (x, t, _, s) ->
      let%map (ctx, effs) = check_stmt (Map.set ctx ~key:x ~data:t, Effect.Set.empty) s in
      let t_ret = lookup_default ctx return_key ~default:Type.Unit in
      Type.Fun (t, t_ret, effs)
  and assert_int ctx v =
    match%bind check_value ctx v with
    | Type.Int -> Ok ()
    | _ -> Or_error.error_s [%message "int assertion failure" (v : Value.t)]
  and check_expr ctx = function
    | Expr.Value v ->
      let%map t = check_value ctx v in
      (t, Effect.Set.empty)
    | Unary (Negate, v) ->
      let%map () = assert_int ctx v in
      (Type.Int, Effect.Set.empty)
    | Binary ((Plus | Minus | Multiply | Divide), v1, v2) ->
      let%bind () = assert_int ctx v1 in
      let%map () = assert_int ctx v2 in
      (Type.Int, Effect.Set.empty)
    | Apply (vf, vs) ->
      let%bind tf = check_value ctx vf in
      let tf_orig = tf in
      let%bind tvs = List.map vs ~f:(check_value ctx) |> Or_error.combine_errors in
      List.fold_result tvs ~init:(tf, Effect.Set.empty) ~f:(fun (tf, effs) tv ->
          match tf with
          | Fun (t_arg, t_ret, effs_f) when Type.equal t_arg tv -> Ok (t_ret, Set.union effs effs_f)
          | _ ->
            Or_error.error_s
              [%message
                "invalid function application"
                  ~func_type:(tf_orig : Type.t)
                  ~args:(tvs : Type.t list)])
  and check_stmt (ctx, all_effs) = function
    | Stmt.Skip -> Ok (ctx, all_effs)
    | Assign (x, t, effs, e) as stmt ->
      let%bind (t_e, effs_e) = check_expr ctx e in
      if Type.equal t t_e && Effect.Set.equal effs effs_e then
        match Map.find ctx x with
        | Some t_ctx when not (Type.equal t t_ctx) ->
          Or_error.error_s [%message "assignment type mismatch" x (t : Type.t) (t_ctx : Type.t)]
        | _ -> Ok (Map.set ctx ~key:x ~data:t, Set.union all_effs effs)
      else
        Or_error.error_s
          [%message
            "assignment annotation mismatch"
              ~assignment:(stmt : Stmt.t)
              ~actual:((t_e, effs_e) : Type.t * Effect.Set.t)]
    | RecAssign (x, t, effs, e) ->
      check_stmt (Map.set ctx ~key:x ~data:t, all_effs) (Assign (x, t, effs, e))
    | If (v, s1, s2) ->
      let%bind () = assert_int ctx v in
      let%bind (ctx1, effs1) = check_stmt (ctx, all_effs) s1 in
      let%bind (ctx2, effs2) = check_stmt (ctx, all_effs) s2 in
      (fun () ->
        ( Map.merge ctx1 ctx2 ~f:(fun ~key:x -> function
            | `Both (t1, t2) ->
              if Type.equal t1 t2 then Some t1
              else raise_s [%message "type mismatch across branches" x (t1 : Type.t) (t2 : Type.t)]
            | _ -> None),
          Effect.Set.union_list [ all_effs; effs1; effs2 ] ))
      |> Or_error.try_with
    | While (v, s) ->
      let%bind () = assert_int ctx v in
      let%map (_, all_effs) = check_stmt (ctx, all_effs) s in
      (ctx, all_effs)
    | Seq ss -> List.fold_result ss ~init:(ctx, all_effs) ~f:check_stmt
    | Return v ->
      let%map t = check_value ctx v in
      (Map.set ctx ~key:return_key ~data:t, all_effs)
  in
  let lib_ctx = Map.map Library.funcs ~f:fst in
  let%map (_ : Type.t String.Map.t * Effect.Set.t) =
    List.fold_result prog ~init:(lib_ctx, Effect.Set.empty) ~f:check_stmt
  in
  ()
