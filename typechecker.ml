open! Core
open! Ast
open! Or_error.Let_syntax

let lookup ctx x =
  match Map.find ctx x with
  | None -> Or_error.error_s [%message "variable not found in type context" x]
  | Some v -> Ok v

let lookup_default ctx x ~default = Map.find ctx x |> Option.value ~default

let rec check_value ctx = function
  | Value.Unit -> Ok Type.Unit
  | Int _ -> Ok Type.Int
  | Bool _ -> Ok Type.Bool
  | Var x -> lookup ctx x
  | Lambda (x, t, _, s) ->
    let%map (ctx, effs) = check_stmt (Map.set ctx ~key:x ~data:t, Effect.Set.empty) s in
    let t_ret = lookup_default ctx return_key ~default:Type.Unit in
    Type.Fun (t, t_ret, effs)
  | Extern _ -> failwith "impossible"

and check_expr ctx = function
  | Expr.Value v ->
    let%map t = check_value ctx v in
    (t, Effect.Set.empty)
  | Apply (e1, e2) ->
    let%bind (t2, effs2) = check_expr ctx e2 in
    ( match%bind check_expr ctx e1 with
    | (Fun (t_arg, t_ret, effs_f), effs1) when Type.equal t_arg t2 ->
      Ok (t_ret, Effect.Set.union_list [ effs1; effs2; effs_f ])
    | _ ->
      Or_error.error_s
        [%message "invalid function application" ~func_expr:(e1 : Expr.t) ~arg_expr:(e2 : Expr.t)]
    )

and assert_bool ctx e =
  match%bind check_expr ctx e with
  | (Type.Bool, effs) -> Ok effs
  | _ -> Or_error.error_s [%message "bool assertion failure" (e : Expr.t)]

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
  | If (e, s1, s2) ->
    let%bind effs_e = assert_bool ctx e in
    let%bind (ctx1, effs1) = check_stmt (ctx, all_effs) s1 in
    let%bind (ctx2, effs2) = check_stmt (ctx, all_effs) s2 in
    (fun () ->
      ( Map.merge ctx1 ctx2 ~f:(fun ~key:x -> function
          | `Both (t1, t2) ->
            if Type.equal t1 t2 then Some t1
            else raise_s [%message "type mismatch across branches" x (t1 : Type.t) (t2 : Type.t)]
          | _ -> None),
        Effect.Set.union_list [ all_effs; effs_e; effs1; effs2 ] ))
    |> Or_error.try_with
  | While (e, s) ->
    let%bind effs = assert_bool ctx e in
    let%map (_, all_effs) = check_stmt (ctx, all_effs) s in
    (ctx, Set.union effs all_effs)
  | Seq ss -> List.fold_result ss ~init:(ctx, all_effs) ~f:check_stmt
  | Return e ->
    let%map (t, effs) = check_expr ctx e in
    (Map.set ctx ~key:return_key ~data:t, Set.union effs all_effs)

let check prog =
  let lib_ctx = Map.map Library.extern ~f:fst in
  let%map (_ : Type.t String.Map.t * Effect.Set.t) =
    List.fold_result prog ~init:(lib_ctx, Effect.Set.empty) ~f:check_stmt
  in
  ()
