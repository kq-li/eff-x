open! Core

let return_key = "*retval"

module Effect = struct
  module T = struct
    type t =
      | Input
      | Output
      | Read
      | Write
    [@@deriving compare, equal, sexp_of]
  end

  include T
  include Comparable.Make_plain (T)

  let to_json = function
    | Input -> `Assoc [("kind", `String "input")]
    | Output -> `Assoc [("kind", `String "output")]
    | Read -> `Assoc [("kind", `String "read")]
    | Write -> `Assoc [("kind", `String "write")]
end

module Type = struct
  type t =
    | Unit
    | Int
    | Bool
    | Fun of t * t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]

  let rec to_json = function
    | Unit -> `Assoc [("kind", `String "unit")]
    | Int -> `Assoc [("kind", `String "int")]
    | Bool -> `Assoc [("kind", `String "bool")]
    | Fun (t1, t2, effs) ->
      `Assoc [
        ("kind", `String "fun");
        ("t1", to_json t1);
        ("t2", to_json t2);
        ("effs", `List (Set.to_list effs |> List.map ~f:Effect.to_json));
      ]
end

module rec Value : sig
  type t =
    | Unit
    | Int of int
    | Bool of bool
    | Var of string
    | Lambda of string * Type.t * Value.t String.Map.t * Stmt.t
  [@@deriving compare, equal, sexp_of]

  val to_json : t -> Yojson.Basic.t
end = struct
  type t =
    | Unit
    | Int of int
    | Bool of bool
    | Var of string
    | Lambda of string * Type.t * Value.t String.Map.t * Stmt.t
  [@@deriving compare, equal, sexp_of]

  let to_json = function
    | Unit -> `Assoc [("kind", `String "unit")]
    | Int n -> `Assoc [("kind", `String "int"); ("value", `Int n)]
    | Bool b -> `Assoc [("kind", `String "bool"); ("value", `Bool b)]
    | Var x -> `Assoc [("kind", `String "var"); ("name", `String x)]
    | Lambda (x, t, _, s) ->
      `Assoc [
        ("kind", `String "lambda");
        ("arg", `String x);
        ("arg_type", Type.to_json t);
        ("body", Stmt.to_json s);
      ]

end

and Expr : sig
  type t =
    | Value of Value.t
    | Apply of t * t
  [@@deriving compare, equal, sexp_of]

  val to_json : t -> Yojson.Basic.t
end = struct
  type t =
    | Value of Value.t
    | Apply of t * t
  [@@deriving compare, equal, sexp_of]

  let to_json = function
    | Value v -> Value.to_json v
    | Apply (e1, e2) -> `Assoc [("kind", `String "apply"); ("e1", Expr.to_json e1); ("e2", Expr.to_json e2)]
end

and Stmt : sig
  type t =
    | Skip
    | Assign of string * Type.t * Effect.Set.t * Expr.t
    | If of Expr.t * t * t
    | While of Expr.t * t
    | For of string * int * int * t
    | CFor of string * int * int * t
    | Seq of t list
    | Return of Expr.t
  [@@deriving compare, equal, sexp_of]

  val to_json : t -> Yojson.Basic.t
end = struct
  type t =
    | Skip
    | Assign of string * Type.t * Effect.Set.t * Expr.t
    | If of Expr.t * t * t
    | While of Expr.t * t
    | For of string * int * int * t
    | CFor of string * int * int * t
    | Seq of t list
    | Return of Expr.t
  [@@deriving compare, equal, sexp_of]

  let rec to_json = function
    | Skip -> `Assoc [("kind", `String "skip")]
    | Assign (x, t, effs, e) ->
      `Assoc [
        ("kind", `String "assign");
        ("var", `String x);
        ("var_type", Type.to_json t);
        ("effs", `List (Set.to_list effs |> List.map ~f:Effect.to_json));
        ("expr", Expr.to_json e);
      ]
    | If (e, s1, s2) ->
      `Assoc [
        ("kind", `String "if");
        ("guard", Expr.to_json e);
        ("true", to_json s1);
        ("false", to_json s2);
      ]
    | While (e, s) ->
      `Assoc [
        ("kind", `String "while");
        ("guard", Expr.to_json e);
        ("body", to_json s);
      ]
    | For (x, a, b, s) ->
      `Assoc [
        ("kind", `String "for");
        ("name", `String x);
        ("start", `Int a);
        ("end", `Int b);
        ("body", to_json s);
      ]
    | CFor (x, a, b, s) ->
      `Assoc [
        ("kind", `String "cfor");
        ("name", `String x);
        ("start", `Int a);
        ("end", `Int b);
        ("body", to_json s);
      ]
    | Seq ss ->
      `Assoc [
        ("kind", `String "seq");
        ("body", `List (List.map ss ~f:to_json));
      ]
    | Return e -> `Assoc [("kind", `String "return"); ("expr", Expr.to_json e)]
end

module Prog = struct
  type t = Stmt.t list [@@deriving compare, equal, sexp_of]

  let to_json t =
    `Assoc [
      ("kind", `String "seq");
      ("body", `List (List.map t ~f:Stmt.to_json));
    ]
end
