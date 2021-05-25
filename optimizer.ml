open! Core
open! Ast
open! Or_error.Let_syntax

let effs_commutative effs = Set.is_empty (Set.inter effs Effect.noncommutative)
let reducers = String.Map.of_alist_exn [ ("add", 0); ("mul", 1) ]

module State = struct
  type t =
    | Read
    | Written of string
    | Init of int option
    | Any
  [@@deriving compare, equal, sexp_of]
end

module Loc = struct
  module T = struct
    type t = string * string list [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

module Loc_opt = struct
  module T = struct
    type t = Loc.t option [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

let rec writes_aux is order = function
  | Assignable.Var x -> Some (x, order)
  | Sub (a, Value (Var i)) when Set.mem is i -> writes_aux (Set.remove is i) (i :: order) a
  | Sub _ -> None

let rec writes_stmt is = function
  | Stmt.Assign ((Sub _ as a), _, _, _) -> writes_aux is [] a |> Loc_opt.Set.singleton
  | If (_, _, s1, s2) -> Set.union (writes_stmt is s1) (writes_stmt is s2)
  | While (_, _, s) -> writes_stmt is s
  | For (i, _, _, _, _, s)
  | CFor (i, _, _, _, _, _, s) ->
    writes_stmt (Set.add is i) s
  | Seq ss -> List.map ss ~f:(writes_stmt is) |> Loc_opt.Set.union_list
  | _ -> Loc_opt.Set.empty

let rec reads_aux is order = function
  | Value.Var x -> Some (x, order)
  | Sub (Value v, Value (Var i)) when Set.mem is i -> reads_aux (Set.remove is i) (i :: order) v
  | _ -> None

let reads_value is = function
  | Value.Sub _ as v -> reads_aux is [] v |> Loc_opt.Set.singleton
  | Lambda (_, _, _, s) when Set.mem (Stmt.all_effs s) Read -> Loc_opt.Set.singleton None
  | _ -> Loc_opt.Set.empty

let rec reads_expr is = function
  | Expr.Value v -> reads_value is v
  | Apply (e1, e2) -> Set.union (reads_expr is e1) (reads_expr is e2)

let rec reads_assignable is = function
  | Assignable.Var _ -> Loc_opt.Set.empty
  | Sub (a, e) -> Set.union (reads_assignable is a) (reads_expr is e)

let rec reads_stmt is = function
  | Stmt.Skip -> Loc_opt.Set.empty
  | Assign (a, _, _, e) -> Set.union (reads_assignable is a) (reads_expr is e)
  | If (e, _, s1, s2) ->
    Loc_opt.Set.union_list [ reads_expr is e; reads_stmt is s1; reads_stmt is s2 ]
  | While (e, _, s) -> Set.union (reads_expr is e) (reads_stmt is s)
  | For (i, e1, e2, e3, _, s)
  | CFor (i, e1, e2, e3, _, _, s) ->
    let is = Set.add is i in
    Loc_opt.Set.union_list (reads_stmt is s :: List.map [ e1; e2; e3 ] ~f:(reads_expr is))
  | Seq ss -> List.map ss ~f:(reads_stmt is) |> Loc_opt.Set.union_list
  | Return (e, _) -> reads_expr is e

(* Need loop variable for writing to array indices
 * Need context of existing variables for inferring accumulator + reducer
 *)
let rec check indices ctx stmt =
  let update_ctx ctx e =
    Expr.used_vars e
    |> Set.fold ~init:ctx ~f:(fun ctx x ->
           match Map.find ctx x with
           | None -> ctx
           | Some (State.Init _) -> Map.set ctx ~key:x ~data:Read
           | Some _ -> Map.set ctx ~key:x ~data:Any)
  in
  match stmt with
  | Stmt.Skip -> ctx
  | Assign (Var x, _, _, e) ->
    (* if x is a written non-local variable, ensure x is only read as part of the update *)
    ( match Map.find ctx x with
    | None -> ctx
    | Some (State.Init (Some n)) ->
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
  | Assign (Sub _, _, _, _) -> ctx
  | If (e, _, s1, s2) -> check indices (check indices (update_ctx ctx e) s1) s2
  | While (e, _, s) -> check indices (update_ctx ctx e) s
  | For (x, _, _, _, _, s)
  | CFor (x, _, _, _, _, _, s) ->
    check (Set.add indices x) ctx s
  | Seq ss -> List.fold ss ~init:ctx ~f:(check indices)
  | Return (e, _) -> update_ctx ctx e

let rec optimize_stmt ctx stmt =
  match stmt with
  | Stmt.If (e, effs, s1, s2) ->
    (Stmt.If (e, effs, optimize_stmt ctx s1 |> fst, optimize_stmt ctx s2 |> fst), ctx)
  | Assign (Var x, _, _, Value (Int n)) as s -> (s, Map.set ctx ~key:x ~data:(Some n))
  | Assign (Var x, _, _, _) as s -> (s, Map.set ctx ~key:x ~data:None)
  | Seq ss ->
    let (ss, ctx) =
      List.fold ss ~init:([], ctx) ~f:(fun (ss, ctx) s ->
          let (s, ctx) = optimize_stmt ctx s in
          (s :: ss, ctx))
    in
    (Seq (List.rev ss), ctx)
  | For (x, e1, e2, e3, effs, s) when effs_commutative (Set.union effs (Stmt.all_effs s)) ->
    let loop_ctx = check (String.Set.singleton x) (Map.map ctx ~f:(fun o -> State.Init o)) s in
    let writes = writes_stmt (String.Set.singleton x) s in
    let reads = reads_stmt (String.Set.singleton x) s in
    (* print_s [%message (writes : Loc_opt.Set.t) (reads : Loc_opt.Set.t)]; *)
    let rec longest l1 l2 =
      match (l1, l2) with
      | ([], l)
      | (l, []) ->
        Some l
      | (x1 :: l1, x2 :: l2) ->
        if String.equal x1 x2 then Option.map (longest l1 l2) ~f:(List.cons x1) else None
    in
    let acc_fs_opt =
      if Set.mem writes None || Set.mem reads None then None
      else
        let f x =
          Loc.Set.map x ~f:Option.value_exn
          |> Set.to_list
          |> String.Map.of_alist_multi
          |> Map.map
               ~f:
                 (List.fold ~init:(Some []) ~f:(fun l1_opt l2 ->
                      Option.bind l1_opt ~f:(fun l1 -> longest l1 l2)))
        in
        let writes = f writes in
        let reads = f reads in
        (* print_s
         *   [%message
         *     (writes : string list option String.Map.t) (reads : string list option String.Map.t)]; *)
        Map.fold writes ~init:(Some []) ~f:(fun ~key:x ~data:loc_opt acc_fs_opt ->
            (* print_s [%message (acc_fs_opt : (string * string * bool) list option)]; *)
            Option.bind acc_fs_opt ~f:(fun acc_fs ->
                if
                  Option.value_map loc_opt ~default:false ~f:(fun loc ->
                      match Map.find reads x with
                      | None -> true
                      | Some None -> false
                      | Some (Some loc') -> List.equal String.equal loc loc')
                then Some ((x, "merge", true) :: acc_fs)
                else None))
    in
    (* print_s
     *   [%message
     *     (acc_fs_opt : (string * string * bool) list option) (loop_ctx : State.t String.Map.t)]; *)
    let acc_fs_opt =
      Map.fold loop_ctx ~init:acc_fs_opt ~f:(fun ~key:acc ~data ->
          Option.bind ~f:(fun acc_fs ->
              match data with
              | Written f -> Some ((acc, f, false) :: acc_fs)
              | Init _ -> Some acc_fs
              | _ -> None))
    in
    (* print_s [%message (acc_fs_opt : (string * string * bool) list option)]; *)
    ( match acc_fs_opt with
    | None -> (stmt, ctx)
    | Some acc_fs -> (CFor (x, e1, e2, e3, effs, acc_fs, s), ctx) )
  | _ -> (stmt, ctx)

let optimize prog =
  match optimize_stmt String.Map.empty (Stmt.Seq prog) with
  | (Stmt.Seq prog, _) -> prog
  | _ -> failwith "impossible"
