open! Core
open! Ast

let make_new_var bound =
  let rec aux i =
    let var = sprintf "x%d" i in
    if Set.mem bound var then aux (i + 1) else var
  in
  aux 0

let rec subst expr x new_expr =
  match expr with
  | Var y when String.equal x y -> new_expr
  | Binary (binop, e1, e2) -> Binary (binop, subst e1 x new_expr, subst e2 x new_expr)
  | Apply (e1, e2) -> Apply (subst e1 x new_expr, subst e2 x new_expr)
  | Lambda (y, e) when not (String.equal x y) ->
    if Set.mem (Ast.free_vars new_expr) x then
      let z = make_new_var (Set.union (Ast.free_vars e) (Ast.free_vars new_expr)) in
      Lambda (z, subst (subst e y (Var z)) x new_expr)
    else Lambda (y, subst e x new_expr)
  | Seq (e1, e2) -> Seq (subst e1 x new_expr, subst e2 x new_expr)
  | _ -> expr

let rec reduce expr =
  match expr with
  | Binary (binop, e1, e2) -> Binary (binop, reduce e1, reduce e2)
  | Apply (Lambda (x, e1), e2) -> subst (reduce e1) x (reduce e2)
  | Apply (e1, e2) ->
    let new_expr = Apply (reduce e1, reduce e2) in
    if Ast.equal expr new_expr then expr else reduce new_expr
  | Lambda (x, e) -> Lambda (x, reduce e)
  | Seq (e1, e2) -> Seq (reduce e1, reduce e2)
  | expr -> expr

let eval =
  let open Or_error.Let_syntax in
  let rec eval = function
    | Value v -> Ok v
    | Var x -> Or_error.error_s [%message "cannot eval free variable" x]
    | Binary (binop, e1, e2) ->
      let%bind v1 = eval e1 in
      let%map v2 = eval e2 in
      let f =
        match binop with
        | Plus -> Int.( + )
        | Minus -> Int.( - )
        | Multiply -> Int.( * )
        | Divide -> Int.( / )
      in
      f v1 v2
    | Apply (Var "print", e) ->
      let%map v = eval e in
      printf "%d\n" v;
      0
    | Seq (e1, e2) ->
      let%bind (_ : int) = eval e1 in
      eval e2
    | expr -> Or_error.error_s [%message "cannot evaluate expr" (expr : Ast.t)]
  in
  Fn.compose eval reduce
