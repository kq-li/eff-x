open! Core

module Op = struct
  type t =
    | Plus
    | Minus
    | Multiply
    | Divide
  [@@deriving compare, equal, sexp_of]
end

module Effect = struct
  module T = struct
    type t = Output [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)
end

module Type = struct
  type t =
    | Int
    | Fun of t * t
    | With_effect of t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]

  let rec unify t1 t2 ~combine =
    let open Or_error.Let_syntax in
    match (t1, t2) with
    | (With_effect (t1, effs1), With_effect (t2, effs2)) ->
      let%map t = unify t1 t2 ~combine in
      With_effect (t, Set.union effs1 effs2)
    | (With_effect (t1, effs), t2)
    | (t1, With_effect (t2, effs)) ->
      let%map t = unify t1 t2 ~combine in
      With_effect (t, effs)
    | _ -> combine (t1, t2)
end

module Expr = struct
  type t =
    | Value of int
    | Var of string
    | Binary of Op.t * t * t
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
    let rec check ctx expr =
      let open Or_error.Let_syntax in
      match expr with
      | Value _ -> Ok Type.Int
      | Var x -> Map.find_or_error ctx x
      | Binary (_, e1, e2) ->
        let%bind t1 = check ctx e1 in
        let%bind t2 = check ctx e2 in
        Type.unify t1 t2 ~combine:(function
            | (Int, Int) -> Ok Type.Int
            | _ -> Or_error.error_s [%message "binary op on non-ints" (expr : t)])
      | Lambda (x, t, e) ->
        let%map t_ret = check (Map.set ctx ~key:x ~data:t) e in
        Type.Fun (t, t_ret)
      | Apply (e1, e2) ->
        let%bind t1 = check ctx e1 in
        let%bind t2 = check ctx e2 in
        Type.unify t1 t2 ~combine:(function
            | (Fun (t_arg, t_ret), t) when Type.equal t_arg t -> Ok t_ret
            | (t1, t2) ->
              Or_error.error_s [%message "invalid function application" (t1 : Type.t) (t2 : Type.t)])
      | Seq (e1, e2) ->
        let%bind t1 = check ctx e1 in
        let%bind t2 = check ctx e2 in
        Type.unify t1 t2 ~combine:(Fn.compose Or_error.return snd)
    in
    check
      (String.Map.singleton
         "print"
         (Type.With_effect (Fun (Int, Int), Effect.Set.singleton Output)))
end
