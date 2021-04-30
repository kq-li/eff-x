open! Core
open! Ast
open! Or_error.Let_syntax

let effs_commutative effs =
  let open Effect in
  Set.is_empty (Set.inter effs Effect.noncommutative)

let reducers = String.Set.of_list [ "add"; "mul" ]

type p =
  | Read
  | Written of string
  | Unused
  | Multiple
[@@deriving compare, equal, sexp_of]

(* Need loop variable for writing to array indices
 * Need context of existing variables for inferring accumulator + reducer
 *)
let rec check ctx stmt =
  let update_ctx ctx e =
    Expr.used_vars e
    |> Set.fold ~init:ctx ~f:(fun ctx x ->
           match Map.find ctx x with
           | None -> ctx
           | Some Unused -> Map.set ctx ~key:x ~data:Read
           | Some _ -> Map.set ctx ~key:x ~data:Multiple)
  in
  match stmt with
  | Stmt.Skip -> ctx
  | Assign (x, _, _, e) ->
    ( match Map.find ctx x with
    | None -> ctx
    | Some Unused ->
      ( match e with
      | Apply (Apply (Value (Var f), Value (Var y)), e)
      | Apply (Apply (Value (Var f), e), Value (Var y)) ->
        let ctx = update_ctx ctx e in
        Map.set
          ctx
          ~key:x
          ~data:
            ( if
              Set.mem reducers f
              && String.equal x y
              (* this condition can be refined *)
              && Set.for_all (Expr.used_vars e) ~f:(Fn.non (Map.mem ctx))
            then Written f
            else Multiple )
      | _ -> update_ctx ctx e |> Map.set ~key:x ~data:Multiple )
    | _ -> update_ctx ctx e |> Map.set ~key:x ~data:Multiple )
  | If (e, _, s1, s2) -> check (check (update_ctx ctx e) s1) s2
  | While (e, _, s) -> check (update_ctx ctx e) s
  | For (_, _, _, s)
  | CFor (_, _, _, _, s) ->
    check ctx s
  | Seq ss -> List.fold ss ~init:ctx ~f:check
  | Return (e, _) -> update_ctx ctx e

(* if x is a written non-local variable, ensure x is only read as part of the update *)
let parallelize ctx body =
  let ctx = check ctx body in
  Map.fold ctx ~init:[] ~f:(fun ~key:acc ~data acc_fs ->
      match data with
      | Written f -> (acc, f) :: acc_fs
      | _ -> acc_fs)

let rec optimize_stmt ctx stmt =
  match stmt with
  | Stmt.If (e, effs, s1, s2) ->
    (Stmt.If (e, effs, optimize_stmt ctx s1 |> fst, optimize_stmt ctx s2 |> fst), ctx)
  | Assign (x, _, _, _) as s -> (s, Map.set ctx ~key:x ~data:Unused)
  | Seq ss ->
    let (ss, ctx) =
      List.fold ss ~init:([], ctx) ~f:(fun (ss, ctx) s ->
          let (s, ctx) = optimize_stmt ctx s in
          (s :: ss, ctx))
    in
    (Seq (List.rev ss), ctx)
  | For (x, a, b, s) when effs_commutative (Stmt.all_effs s) ->
    (CFor (x, a, b, parallelize ctx s, s), ctx)
  | s -> (s, ctx)

let optimize prog =
  match optimize_stmt String.Map.empty (Stmt.Seq prog) with
  | (Stmt.Seq prog, _) -> prog
  | _ -> failwith "impossible"
