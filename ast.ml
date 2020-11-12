open! Core

type op =
  | Plus
  | Minus
  | Multiply
  | Divide
[@@deriving compare, equal, sexp_of]

module Type = struct
  type t =
    | Int
    | Fun of t * t
  [@@deriving compare, equal, sexp_of]
end

module Expr = struct
  type t =
    | Value of int
    | Var of string
    | Binary of op * t * t
    | Lambda of string * Type.t * t
    | Apply of t * t
    | Seq of t * t
  [@@deriving compare, equal, sexp_of]

  let is_value = function
    | Value _ -> true
    | _ -> false

  let value_exn = function
    | Value v -> v
    | expr -> raise_s [%message "not a value" (expr : t)]

  let rec free_vars = function
    | Value _ -> String.Set.empty
    | Var x -> String.Set.singleton x
    | Lambda (x, _, e) -> Set.remove (free_vars e) x
    | Binary (_, e1, e2)
    | Apply (e1, e2)
    | Seq (e1, e2) ->
      Set.union (free_vars e1) (free_vars e2)

  let check_type =
    let rec check_type ctx expr =
      let open Or_error.Let_syntax in
      match expr with
      | Value _ -> Ok Type.Int
      | Var x -> Map.find_or_error ctx x
      | Binary (_, e1, e2) ->
        let%bind t1 = check_type ctx e1 in
        let%bind t2 = check_type ctx e2 in
        ( match (t1, t2) with
        | (Int, Int) -> Ok Type.Int
        | _ -> Or_error.error_s [%message "binary op on non-ints" (expr : t)] )
      | Lambda (x, t, e) ->
        let%map t' = check_type (Map.set ctx ~key:x ~data:t) e in
        Type.Fun (t, t')
      | Apply (e1, e2) ->
        let%bind t1 = check_type ctx e1 in
        let%bind t2 = check_type ctx e2 in
        ( match t1 with
        | Fun (t, t') when Type.equal t t2 -> Ok t'
        | _ -> Or_error.error_s [%message "invalid function application" (expr : t)] )
      | Seq (e1, e2) ->
        let%bind (_ : Type.t) = check_type ctx e1 in
        check_type ctx e2
    in
    check_type (String.Map.singleton "print" (Type.Fun (Int, Int)))
end
