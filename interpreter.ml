open! Core
open! Ast

let make_new_var bound =
  let rec aux i =
    let var = sprintf "x%d" i in
    if Set.mem bound var then aux (i + 1) else var
  in
  aux 0

let rec subst (expr : Expr.t) x (new_expr : Expr.t) =
  match expr with
  | Var y when String.equal x y -> new_expr
  | Binary (binop, e1, e2) -> Binary (binop, subst e1 x new_expr, subst e2 x new_expr)
  | Apply (e1, e2) -> Apply (subst e1 x new_expr, subst e2 x new_expr)
  | Lambda (y, t, e) when not (String.equal x y) ->
    if Set.mem (Expr.free_vars new_expr) x then
      let z = make_new_var (Set.union (Expr.free_vars e) (Expr.free_vars new_expr)) in
      Lambda (z, t, subst (subst e y (Var z)) x new_expr)
    else Lambda (y, t, subst e x new_expr)
  | Seq (e1, e2) -> Seq (subst e1 x new_expr, subst e2 x new_expr)
  | _ -> expr

let rec reduce (expr : Expr.t) =
  let new_expr : Expr.t =
    match expr with
    | Binary (binop, e1, e2) -> Binary (binop, reduce e1, reduce e2)
    | Apply (Lambda (x, _, e1), e2) -> subst (reduce e1) x (reduce e2)
    | Apply (e1, e2) -> Apply (reduce e1, reduce e2)
    | Lambda (x, t, e) -> Lambda (x, t, reduce e)
    | Seq (e1, e2) -> Seq (reduce e1, reduce e2)
    | expr -> expr
  in
  if Expr.equal expr new_expr then expr else reduce new_expr

let eval =
  let rec eval (expr : Expr.t) =
    let open Or_error.Let_syntax in
    let error = Or_error.error_s [%message "cannot evaluate expr" (expr : Expr.t)] in
    match expr with
    | Value _
    | Var _ ->
      Ok expr
    | Binary (binop, e1, e2) ->
      let%bind e1 = eval e1 in
      let%bind e2 = eval e2 in
      ( match (e1, e2) with
      | (Value v1, Value v2) ->
        let f =
          match binop with
          | Plus -> Int.( + )
          | Minus -> Int.( - )
          | Multiply -> Int.( * )
          | Divide -> Int.( / )
        in
        Ok (Expr.Value (f v1 v2))
      | _ -> error )
    | Apply (Var "print", e) ->
      ( match%bind eval e with
      | Value v ->
        printf "%d\n" v;
        Ok (Expr.Value 0)
      | _ -> error )
    | Apply (e1, e2) ->
      let%bind e1' = eval e1 in
      let%bind e2' = eval e2 in
      if Expr.equal e1 e1' && Expr.equal e2 e2' then Ok expr else Apply (e1', e2') |> reduce |> eval
    | Seq (e1, e2) ->
      let%bind (_ : Expr.t) = eval e1 in
      eval e2
    | _ -> Ok expr
  in
  Fn.compose eval reduce
