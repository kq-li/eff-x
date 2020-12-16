open! Core
open! Ast

let check prog =
  (let open Or_error.Let_syntax in
  let lib_func_types =
    Map.map Library.funcs ~f:(fun (t_args, t_ret, effs, _) -> (t_args, t_ret, effs))
  in
  let prog_func_types =
    Map.map prog ~f:(fun Func.{ args; ret_type; effects; _ } ->
        (Some (List.map args ~f:snd), ret_type, effects))
  in
  let%bind func_types =
    (fun () ->
      Map.merge lib_func_types prog_func_types ~f:(fun ~key:f -> function
        | `Left t
        | `Right t ->
          Some t
        | `Both _ -> raise_s [%message "shadowed library function" f]))
    |> Or_error.try_with
  in
  let check_value ctx = function
    | Value.Unit -> Ok Type.Unit
    | Int _ -> Ok Type.Int
    | Var x ->
      ( match Map.find ctx x with
      | Some t -> Ok t
      | None -> Or_error.error_s [%message "variable not found" x] )
  in
  let assert_int ctx v =
    match%bind check_value ctx v with
    | Type.Int -> Ok ()
    | _ -> Or_error.error_s [%message "int assertion failure" (v : Value.t)]
  in
  let check_expr ctx = function
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
    | Apply (f, vs) ->
      let%bind t_vs = List.map vs ~f:(check_value ctx) |> Or_error.combine_errors in
      let%bind (t_args, t_ret, effs) = Map.find_or_error func_types f in
      ( match t_args with
      | Some t_args when not (List.equal Type.equal t_vs t_args) ->
        Or_error.error_s
          [%message "function argument mismatch" (t_vs : Type.t list) (t_args : Type.t list)]
      | _ -> Ok (t_ret, effs) )
  in
  let rec check_stmt (ctx, all_effs) = function
    | Stmt.Skip -> Ok (ctx, all_effs)
    | Assign (x, t, effs, e) ->
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
              (t : Type.t)
              (effs : Effect.Set.t)
              (t_e : Type.t)
              (effs_e : Effect.Set.t)]
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
  let check_func Func.{ body; ret_type; effects; _ } =
    let ctx = String.Map.singleton return_key ret_type in
    let%bind (ctx, effs) = check_stmt (ctx, Effect.Set.empty) body in
    let t_ret = Map.find ctx return_key |> Option.value ~default:Type.Unit in
    if Type.equal t_ret ret_type && Effect.Set.equal effs effects then Ok ()
    else
      Or_error.error_s
        [%message
          "function type mismatch"
            (t_ret : Type.t)
            (effs : Effect.Set.t)
            (ret_type : Type.t)
            (effects : Effect.Set.t)]
  in
  Map.data prog |> List.map ~f:check_func |> Or_error.combine_errors_unit)
  |> ok_exn
