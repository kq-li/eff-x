open! Core
open! Ast
open! Or_error.Let_syntax

let empty_ctx = String.Map.empty
let lookup_opt ctx x = Map.find ctx x

let lookup ~lookup_opt ctx x =
  match lookup_opt ctx x with
  | None -> Or_error.error_s [%message "variable not found" x (ctx : Value.t String.Map.t)]
  | Some v -> Ok v

let extract ~lookup_opt ctx = function
  | Value.Var x ->
    ( match Map.find Library.extern x with
    | Some (_, v) -> Ok (Value.Extern v)
    | _ -> lookup ~lookup_opt ctx x )
  | value -> Ok value

let merge ctx ctx' =
  (fun () ->
    Map.merge ctx ctx' ~f:(fun ~key:var -> function
      | `Left v
      | `Right v ->
        Some v
      | `Both (v, v') ->
        if Value.equal v v' then Some v else raise_s [%message "shadowed variable in closure" var]))
  |> Or_error.try_with

let rec eval_expr ?(lookup_opt = lookup_opt) ctx = function
  | Expr.Value v -> extract ~lookup_opt ctx v
  | Apply (e1, e2) ->
    let%bind v1 = eval_expr ~lookup_opt ctx e1 in
    let%bind v2 = eval_expr ~lookup_opt ctx e2 in
    ( match v1 with
    | Value.Lambda (x, _, ctx1, s) ->
      let ctx1 = Map.set ctx1 ~key:x ~data:v2 in
      let%bind ctx_ret = eval_stmt ~lookup_opt ctx1 s in
      let%map v_ret =
        match lookup_opt ctx_ret return_key with
        | None -> Ok Value.Unit
        | Some (Value.Lambda (x, t, ctx2, s)) ->
          let%map ctx = merge ctx1 ctx2 in
          Value.Lambda (x, t, ctx, s)
        | Some v -> Ok v
      in
      v_ret
    | Extern f -> Ok (f v2)
    | _ -> Or_error.error_s [%message "invalid function application" (e1 : Expr.t) (e2 : Expr.t)] )

and bool_of_expr ?lookup_opt ctx e =
  match%bind eval_expr ?lookup_opt ctx e with
  | Value.Bool b -> Ok b
  | _ -> Or_error.error_s [%message "bool conversion failure" (e : Expr.t)]

and eval_stmt ?lookup_opt ctx = function
  | Stmt.Skip -> Ok ctx
  | Assign (x, _, _, e) ->
    let%map v = eval_expr ?lookup_opt ctx e in
    Map.set ctx ~key:x ~data:v
  | If (e, s1, s2) ->
    if%bind bool_of_expr ?lookup_opt ctx e then eval_stmt ?lookup_opt ctx s1
    else eval_stmt ?lookup_opt ctx s2
  | While (e, s) as loop ->
    if%bind bool_of_expr ?lookup_opt ctx e then
      let%bind ctx = eval_stmt ?lookup_opt ctx s in
      eval_stmt ?lookup_opt ctx loop
    else Ok ctx
  | Seq ss ->
    let%map (_, ctx) =
      List.fold_result ss ~init:(true, ctx) ~f:(fun (continue, ctx) stmt ->
          let continue =
            match stmt with
            | Stmt.Return _ -> false
            | _ -> continue
          in
          let%map ctx = eval_stmt ?lookup_opt ctx stmt in
          (continue, ctx))
    in
    ctx
  | Return e ->
    let%bind v = eval_expr ?lookup_opt ctx e in
    Ok (Map.set ctx ~key:return_key ~data:v)

let eval ?lookup_opt prog =
  let%map (_ : Value.t String.Map.t) = eval_stmt ?lookup_opt empty_ctx (Stmt.Seq prog) in
  ()
