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
  | Value.Unit -> Ok Type.Unit
  | Int _ -> Ok Type.Int
  | Bool _ -> Ok Type.Bool
  | Var x -> lookup ctx x
  | Lambda (x, t, _, s) ->
    let%map (ctx, effs) = check_stmt (set_type ctx x t, Effect.Set.empty) s in
    let t_ret = lookup_default ctx return_key ~default:Type.Unit in
    Type.Fun (t, t_ret, effs)

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

and assert_type t1 t2 stmt =
  if Type.equal t1 t2 then Ok ()
  else
    Or_error.error_s [%message "type annotation mismatch" (stmt : Stmt.t) ~expected:(t2 : Type.t)]

and assert_effs effs1 effs2 stmt =
  if Effect.Set.equal effs1 effs2 then Ok ()
  else
    Or_error.error_s
      [%message "effect annotation mismatch" (stmt : Stmt.t) ~expected:(effs2 : Effect.Set.t)]

and check_stmt (ctx, all_effs) stmt : (Type_or_index.t String.Map.t * Effect.Set.t) Or_error.t =
  match stmt with
  | Stmt.Skip -> Ok (ctx, all_effs)
  | Assign (x, t, effs, e) ->
    let%bind (t_e, effs_e) = check_expr ctx e in
    let%bind () = assert_type t t_e stmt in
    let%bind () = assert_effs effs effs_e stmt in
    ( match Map.find ctx x with
    | Some Index -> Or_error.error_s [%message "cannot reassign loop variable" (stmt : Stmt.t)]
    | Some (Type t_ctx) when not (Type.equal t t_ctx || String.equal x "_") ->
      Or_error.error_s
        [%message "assignment type mismatch" (stmt : Stmt.t) (t : Type.t) (t_ctx : Type.t)]
    | _ -> Ok (set_type ctx x t, Set.union all_effs effs) )
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
  | For (x, _, _, s) ->
    let%bind ctx =
      match Map.find ctx x with
      | Some _ -> Or_error.error_s [%message "loop variable already bound" x]
      | None -> Ok (set_index ctx x)
    in
    check_stmt (ctx, all_effs) s
  | CFor (x, _, _, acc_fs, s) ->
    let%bind ctx =
      match Map.find ctx x with
      | Some _ -> Or_error.error_s [%message "loop variable already bound" x]
      | None -> Ok (set_index ctx x)
    in
    let%bind () =
      List.map acc_fs ~f:(fun (acc, f) ->
          let%bind t_acc = lookup ctx acc in
          let%bind t_f = lookup ctx f in
          let%bind () = assert_type t_acc Type.Int stmt in
          assert_type
            t_f
            (Type.Fun (Int, Type.Fun (Int, Int, Effect.Set.empty), Effect.Set.empty))
            stmt)
      |> Or_error.combine_errors_unit
    in
    check_stmt (ctx, all_effs) s
  | Seq ss -> List.fold_result ss ~init:(ctx, all_effs) ~f:check_stmt
  | Return (e, effs) ->
    let%bind (t, effs_e) = check_expr ctx e in
    let%map () = assert_effs effs effs_e stmt in
    (set_type ctx return_key t, Set.union effs all_effs)

let check prog =
  let%map (_ : Type_or_index.t String.Map.t * Effect.Set.t) =
    check_stmt
      (Map.map Library.extern ~f:(fun t -> Type_or_index.Type t), Effect.Set.empty)
      (Stmt.Seq prog)
  in
  ()
