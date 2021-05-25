open! Core
open! Ast
open! Or_error.Let_syntax

module Type_or_index = struct
  module T = struct
    type t =
      | Type of Type.t
      | Index
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

let no_eff = Effect.Set.empty
let set_type ctx x t = Map.set ctx ~key:x ~data:(Type_or_index.Type t)
let set_index ctx x = Map.set ctx ~key:x ~data:Type_or_index.Index

let lookup ctx x =
  match Map.find ctx x with
  | None -> Or_error.error_s [%message "variable not found in type context" x]
  | Some Type_or_index.Index -> Ok Type.Int
  | Some (Type t) -> Ok t

let lookup_default ctx x ~default =
  match lookup ctx x with
  | Ok t -> t
  | _ -> default

let rec check_value ctx = function
  | Value.Unit -> Ok (Type.Unit, no_eff)
  | Int _ -> Ok (Int, no_eff)
  | Bool _ -> Ok (Bool, no_eff)
  | Sub (e1, e2) ->
    let%bind (t1, effs1) = check_expr ctx e1 in
    ( match t1 with
    | Type.Array (Some t_arr) ->
      let%map effs2 = assert_int ctx e2 in
      (t_arr, Effect.Set.union_list [ Effect.Set.singleton Read; effs1; effs2 ])
    | _ ->
      Or_error.error_s
        [%message "index into invalid array" (e1 : Expr.t) (t1 : Type.t) (e2 : Expr.t)] )
  (* | Array es ->
   *   ( match%map
   *       List.fold_result es ~init:None ~f:(fun acc e ->
   *           match acc with
   *           | None ->
   *             let%map t_effs = check_expr ctx e in
   *             Some t_effs
   *           | Some (t1, effs1) ->
   *             let%bind (t2, effs2) = check_expr ctx e in
   *             if Type.equal t1 t2 then Ok (Some (t1, Set.union effs1 effs2))
   *             else
   *               Or_error.error_s
   *                 [%message
   *                   "array contains multiple types" (es : Expr.t list) (t1 : Type.t) (t2 : Type.t)])
   *     with
   *   | None -> (Type.Array None, no_eff)
   *   | Some (t, effs) -> (Type.Array (Some t), effs) ) *)
  | Var x ->
    let%map t = lookup ctx x in
    (t, no_eff)
  | Lambda (x, t, _, s) ->
    let%map (ctx, effs) = check_stmt (set_type ctx x t, no_eff) s in
    let t_ret = lookup_default ctx return_key ~default:Type.Unit in
    (Type.Fun (t, t_ret, effs), no_eff)

and check_expr ctx = function
  | Expr.Value v -> check_value ctx v
  | Apply (e1, e2) ->
    let%bind (t2, effs2) : Type.t * Effect.Set.t = check_expr ctx e2 in
    ( match%bind check_expr ctx e1 with
    | (Fun (t_arg, t_ret, effs_f), effs1) when Type.equal t_arg t2 ->
      Ok (t_ret, Effect.Set.union_list [ effs1; effs2; effs_f ])
    | _ ->
      Or_error.error_s
        [%message "invalid function application" ~func_expr:(e1 : Expr.t) ~arg_expr:(e2 : Expr.t)]
    )

and assert_int ctx e =
  match%bind check_expr ctx e with
  | (Type.Int, effs) -> Ok effs
  | _ -> Or_error.error_s [%message "int assertion failure" (e : Expr.t)]

and assert_bool ctx e =
  match%bind check_expr ctx e with
  | (Type.Bool, effs) -> Ok effs
  | _ -> Or_error.error_s [%message "bool assertion failure" (e : Expr.t)]

and assert_type t1 t2 stmt =
  match (t1, t2) with
  | _ when Type.equal t1 t2 -> Ok ()
  | (Type.Array (Some _), Type.Array None) -> Ok ()
  | _ ->
    Or_error.error_s [%message "type annotation mismatch" (stmt : Stmt.t) ~expected:(t2 : Type.t)]

and assert_effs effs1 effs2 stmt =
  if Effect.Set.equal effs1 effs2 then Ok ()
  else
    Or_error.error_s
      [%message "effect annotation mismatch" (stmt : Stmt.t) ~expected:(effs2 : Effect.Set.t)]

and check_assignable ctx a t =
  match a with
  | Assignable.Var x ->
    ( match Map.find ctx x with
    | Some Type_or_index.Index ->
      Or_error.error_s [%message "cannot reassign loop variable" ~var:(x : string)]
    | Some (Type t_ctx) when not (Type.equal t t_ctx || String.equal x "_") ->
      Or_error.error_s [%message "assignment type mismatch" x (t : Type.t) (t_ctx : Type.t)]
    | _ -> Ok (set_type ctx x t, Effect.Set.empty) )
  | Sub (a, e) ->
    let%bind (ctx, effs_a) = check_assignable ctx a (Array (Some t)) in
    let%map effs_e = assert_int ctx e in
    (ctx, Effect.Set.union_list [ Effect.Set.singleton Write; effs_a; effs_e ])

and check_stmt (ctx, all_effs) stmt =
  match stmt with
  | Stmt.Skip -> Ok (ctx, all_effs)
  | Assign (a, t, effs_annot, e) ->
    let%bind (ctx, effs_a) = check_assignable ctx a t in
    let%bind (t_e, effs_e) = check_expr ctx e in
    let effs = Set.union effs_a effs_e in
    let%bind () = assert_type t t_e stmt in
    let%map () = assert_effs effs_annot effs stmt in
    (ctx, Set.union all_effs effs)
  | If (e, effs, s1, s2) ->
    let%bind effs_e = assert_bool ctx e in
    let%bind () = assert_effs effs effs_e stmt in
    let%bind (ctx1, effs1) = check_stmt (ctx, all_effs) s1 in
    let%bind (ctx2, effs2) = check_stmt (ctx, all_effs) s2 in
    (fun () ->
      ( Map.merge ctx1 ctx2 ~f:(fun ~key:x -> function
          | `Both (t1, t2) ->
            if Type_or_index.equal t1 t2 then Some t1
            else
              raise_s
                [%message
                  "type mismatch across branches" x (t1 : Type_or_index.t) (t2 : Type_or_index.t)]
          | _ -> None),
        Effect.Set.union_list [ all_effs; effs_e; effs1; effs2 ] ))
    |> Or_error.try_with
  | While (e, effs, s) ->
    let%bind effs_e = assert_bool ctx e in
    let%bind () = assert_effs effs effs_e stmt in
    let%map (_, all_effs) = check_stmt (ctx, all_effs) s in
    (ctx, Set.union effs all_effs)
  | For (x, e1, e2, e3, effs, s) ->
    let%bind effs1 = assert_int ctx e1 in
    let%bind effs2 = assert_int ctx e2 in
    let%bind effs3 = assert_int ctx e3 in
    let%bind () = assert_effs effs (Effect.Set.union_list [ effs1; effs2; effs3 ]) stmt in
    let%bind new_ctx =
      match Map.find ctx x with
      | Some _ -> Or_error.error_s [%message "loop variable already bound" x]
      | None -> Ok (set_index ctx x)
    in
    let%map (_, all_effs) = check_stmt (new_ctx, all_effs) s in
    (ctx, Set.union effs all_effs)
  | CFor (x, e1, e2, e3, effs, _, s) ->
    let%bind effs1 = assert_int ctx e1 in
    let%bind effs2 = assert_int ctx e2 in
    let%bind effs3 = assert_int ctx e3 in
    let%bind () = assert_effs effs (Effect.Set.union_list [ effs1; effs2; effs3 ]) stmt in
    let%bind new_ctx =
      match Map.find ctx x with
      | Some _ -> Or_error.error_s [%message "loop variable already bound" x]
      | None -> Ok (set_index ctx x)
    in
    (* let%bind () =
     *   List.map acc_fs ~f:(fun (acc, f, merge_arr) ->
     *       let%bind t_acc = lookup new_ctx acc in
     *       let%bind t_f = lookup new_ctx f in
     *       let%bind () = assert_type t_acc Type.Int stmt in
     *       assert_type t_f (Type.Fun (Int, Type.Fun (Int, Int, no_eff), no_eff)) stmt)
     *   |> Or_error.combine_errors_unit
     * in *)
    let%map (_, all_effs) = check_stmt (new_ctx, all_effs) s in
    (ctx, Set.union effs all_effs)
  | Seq ss -> List.fold_result ss ~init:(ctx, all_effs) ~f:check_stmt
  | Return (e, effs) ->
    let%bind (t, effs_e) = check_expr ctx e in
    let%map () = assert_effs effs effs_e stmt in
    (set_type ctx return_key t, Set.union effs all_effs)

let check prog =
  let%map (_ : Type_or_index.t String.Map.t * Effect.Set.t) =
    check_stmt (Map.map Library.extern ~f:(fun t -> Type_or_index.Type t), no_eff) (Stmt.Seq prog)
  in
  ()
