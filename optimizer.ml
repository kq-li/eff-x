open! Core
open! Ast
open! Or_error.Let_syntax

let rec optimize_stmt ctx stmt =
  match stmt with
  | Stmt.Assign (x, t, effs, e) when Set.is_empty effs ->
    ( match Interpreter.eval_expr ctx e with
    | Ok v -> (Stmt.Assign (x, t, effs, Expr.Value v), Map.set ctx ~key:x ~data:v)
    | Error _ -> (stmt, ctx) )
  | If (e, s1, s2) ->
    let (s1, ctx1) = optimize_stmt ctx s1 in
    let (s2, ctx2) = optimize_stmt ctx s2 in
    ( If (e, s1, s2),
      Map.merge ctx1 ctx2 ~f:(fun ~key:_ -> function
        | `Both (v1, v2) when Value.equal v1 v2 -> Some v1
        | _ -> None) )
  | While (e, s) ->
    let (s, ctx) = optimize_stmt String.Map.empty s in
    (While (e, s), ctx)
  | Seq ss ->
    let (ss_rev, ctx) =
      List.fold ss ~init:([], ctx) ~f:(fun (ss, ctx) s ->
          let (s, ctx) = optimize_stmt ctx s in
          (s :: ss, ctx))
    in
    (Stmt.Seq (List.rev ss_rev), ctx)
  | _ -> (stmt, ctx)

let optimize prog =
  match optimize_stmt String.Map.empty (Stmt.Seq prog) with
  | (Stmt.Seq prog, _) -> prog
  | _ -> failwith "impossible"
