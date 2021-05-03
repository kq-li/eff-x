open! Core
open! Ast
open! Or_error.Let_syntax

let effs_commutative effs =
  let open Effect in
  Set.is_empty (Set.inter effs Effect.noncommutative)

let reducers = String.Map.of_alist_exn [ ("add", 0); ("mul", 1) ]

type p =
  | Read
  | Written of string
  | Init of int
  | Any
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
           | Some (Init _) -> Map.set ctx ~key:x ~data:Read
           | Some _ -> Map.set ctx ~key:x ~data:Any)
  in
  match stmt with
  | Stmt.Skip -> ctx
  | Assign (x, _, _, e) ->
    (* if x is a written non-local variable, ensure x is only read as part of the update *)
    ( match Map.find ctx x with
    | None -> ctx
    | Some (Init n) ->
      ( match e with
      | Apply (Apply (Value (Var f), Value (Var y)), e)
      | Apply (Apply (Value (Var f), e), Value (Var y)) ->
        let ctx = update_ctx ctx e in
        Map.set
          ctx
          ~key:x
          ~data:
            ( if
              Map.find reducers f |> Option.value_map ~f:(( = ) n) ~default:false
              && String.equal x y
              (* this condition can be refined *)
              && Set.for_all (Expr.used_vars e) ~f:(Fn.non (Map.mem ctx))
            then Written f
            else Any )
      | _ -> update_ctx ctx e |> Map.set ~key:x ~data:Any )
    | _ -> update_ctx ctx e |> Map.set ~key:x ~data:Any )
  | If (e, _, s1, s2) -> check (check (update_ctx ctx e) s1) s2
  | While (e, _, s) -> check (update_ctx ctx e) s
  | For (_, _, _, _, _, s)
  | CFor (_, _, _, _, _, _, s) ->
    check ctx s
  | Seq ss -> List.fold ss ~init:ctx ~f:check
  | Return (e, _) -> update_ctx ctx e

let rec optimize_stmt ctx stmt =
  match stmt with
  | Stmt.If (e, effs, s1, s2) ->
    (Stmt.If (e, effs, optimize_stmt ctx s1 |> fst, optimize_stmt ctx s2 |> fst), ctx)
  | Assign (x, _, _, Value (Int n)) as s -> (s, Map.set ctx ~key:x ~data:(Some n))
  | Assign (x, _, _, _) as s -> (s, Map.set ctx ~key:x ~data:None)
  | Seq ss ->
    let (ss, ctx) =
      List.fold ss ~init:([], ctx) ~f:(fun (ss, ctx) s ->
          let (s, ctx) = optimize_stmt ctx s in
          (s :: ss, ctx))
    in
    (Seq (List.rev ss), ctx)
  | For (x, a, b, s) when effs_commutative (Stmt.all_effs s) ->
    let loop_ctx =
      check
        (Map.map ctx ~f:(function
            | None -> Any
            | Some n -> Init n))
        s
    in
    let acc_fs_opt =
      Map.fold loop_ctx ~init:(Some []) ~f:(fun ~key:acc ~data ->
          Option.bind ~f:(fun acc_fs ->
              match data with
              | Written f -> Some ((acc, f) :: acc_fs)
              | _ -> None))
    in
    ( match acc_fs_opt with
    | None -> (stmt, ctx)
    | Some acc_fs -> (CFor (x, a, b, acc_fs, s), ctx) )
  | _ -> (stmt, ctx)

let optimize prog =
  match optimize_stmt String.Map.empty (Stmt.Seq prog) with
  | (Stmt.Seq prog, _) -> prog
  | _ -> failwith "impossible"
