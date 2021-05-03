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

  let noncommutative = Set.of_list [ Input; Output; Write ]

  let to_json = function
    | Input -> `Assoc [ ("kind", `String "input") ]
    | Output -> `Assoc [ ("kind", `String "output") ]
    | Read -> `Assoc [ ("kind", `String "read") ]
    | Write -> `Assoc [ ("kind", `String "write") ]

  module Set = struct
    include Set

    let to_json t = `List (Set.to_list t |> List.map ~f:to_json)
  end
end

module Type = struct
  type t =
    | Unit
    | Int
    | Bool
    | Array of t option
    | Fun of t * t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]

  let rec to_json = function
    | Unit -> `Assoc [ ("kind", `String "unit") ]
    | Int -> `Assoc [ ("kind", `String "int") ]
    | Bool -> `Assoc [ ("kind", `String "bool") ]
    | Array t ->
      `Assoc [ ("kind", `String "array"); ("t", Option.value_map t ~f:to_json ~default:`Null) ]
    | Fun (t1, t2, effs) ->
      `Assoc
        [
          ("kind", `String "fun");
          ("t1", to_json t1);
          ("t2", to_json t2);
          ("effs", Effect.Set.to_json effs);
        ]
end

module rec Value : sig
  type t =
    | Unit
    | Int of int
    | Bool of bool
    | Sub of Expr.t * Expr.t
    | Array of Expr.t list
    | Var of string
    | Lambda of string * Type.t * Value.t String.Map.t * Stmt.t
  [@@deriving compare, equal, sexp_of]

  val used_vars : t -> String.Set.t
  val to_json : t -> Yojson.Basic.t
end = struct
  type t =
    | Unit
    | Int of int
    | Bool of bool
    | Sub of Expr.t * Expr.t
    | Array of Expr.t list
    | Var of string
    | Lambda of string * Type.t * Value.t String.Map.t * Stmt.t
  [@@deriving compare, equal, sexp_of]

  let used_vars = function
    | Var x -> String.Set.singleton x
    | Array es -> List.map es ~f:Expr.used_vars |> String.Set.union_list
    | _ -> String.Set.empty

  let to_json = function
    | Unit -> `Assoc [ ("kind", `String "unit") ]
    | Int n -> `Assoc [ ("kind", `String "int"); ("value", `Int n) ]
    | Bool b -> `Assoc [ ("kind", `String "bool"); ("value", `Bool b) ]
    | Sub (e1, e2) ->
      `Assoc [ ("kind", `String "sub"); ("e1", Expr.to_json e1); ("e2", Expr.to_json e2) ]
    | Array es ->
      `Assoc [ ("kind", `String "array"); ("exprs", `List (List.map es ~f:Expr.to_json)) ]
    | Var x -> `Assoc [ ("kind", `String "var"); ("name", `String x) ]
    | Lambda (x, t, _, s) ->
      `Assoc
        [
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

  val used_vars : t -> String.Set.t
  val to_json : t -> Yojson.Basic.t
end = struct
  type t =
    | Value of Value.t
    | Apply of t * t
  [@@deriving compare, equal, sexp_of]

  let rec used_vars = function
    | Value v -> Value.used_vars v
    | Apply (e1, e2) -> Set.union (used_vars e1) (used_vars e2)

  let to_json = function
    | Value v -> Value.to_json v
    | Apply (e1, e2) ->
      `Assoc [ ("kind", `String "apply"); ("e1", Expr.to_json e1); ("e2", Expr.to_json e2) ]
end

and Assignable : sig
  type t =
    | Var of string
    | Sub of t * Expr.t
  [@@deriving compare, equal, sexp_of]

  val used_vars : t -> String.Set.t
  val to_json : t -> Yojson.Basic.t
end = struct
  type t =
    | Var of string
    | Sub of t * Expr.t
  [@@deriving compare, equal, sexp_of]

  let rec used_vars = function
    | Var _ -> String.Set.empty
    | Sub (t, e) -> Set.union (used_vars t) (Expr.used_vars e)

  let rec to_json = function
    | Var x -> `Assoc [ ("kind", `String "var"); ("name", `String x) ]
    | Sub (t, e) ->
      `Assoc [ ("kind", `String "sub"); ("base", to_json t); ("index", Expr.to_json e) ]
end

and Stmt : sig
  type t =
    | Skip
    | Assign of Assignable.t * Type.t * Effect.Set.t * Expr.t
    | If of Expr.t * Effect.Set.t * t * t
    | While of Expr.t * Effect.Set.t * t
    | For of string * Expr.t * Expr.t * Expr.t * Effect.Set.t * t
    | CFor of string * Expr.t * Expr.t * Expr.t * Effect.Set.t * (string * string) list * t
    | Seq of t list
    | Return of Expr.t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]

  val all_effs : t -> Effect.Set.t
  val used_vars : t -> String.Set.t
  val to_json : t -> Yojson.Basic.t
end = struct
  type t =
    | Skip
    | Assign of Assignable.t * Type.t * Effect.Set.t * Expr.t
    | If of Expr.t * Effect.Set.t * t * t
    | While of Expr.t * Effect.Set.t * t
    | For of string * Expr.t * Expr.t * Expr.t * Effect.Set.t * t
    | CFor of string * Expr.t * Expr.t * Expr.t * Effect.Set.t * (string * string) list * t
    | Seq of t list
    | Return of Expr.t * Effect.Set.t
  [@@deriving compare, equal, sexp_of]

  let rec all_effs = function
    | Skip -> Effect.Set.empty
    | Assign (_, _, effs, _) -> effs
    | If (_, effs, s1, s2) -> Effect.Set.union_list [ effs; all_effs s1; all_effs s2 ]
    | While (_, effs, s) -> Set.union effs (all_effs s)
    | For (_, _, _, _, effs, s)
    | CFor (_, _, _, _, effs, _, s) ->
      Set.union effs (all_effs s)
    | Seq ss -> Effect.Set.union_list (List.map ss ~f:all_effs)
    | Return (_, effs) -> effs

  let rec used_vars = function
    | Skip -> String.Set.empty
    | Assign (a, _, _, e) -> Set.union (Assignable.used_vars a) (Expr.used_vars e)
    | If (e, _, s1, s2) -> String.Set.union_list [ Expr.used_vars e; used_vars s1; used_vars s2 ]
    | While (e, _, s) -> Set.union (Expr.used_vars e) (used_vars s)
    | For (_, e1, e2, e3, _, s)
    | CFor (_, e1, e2, e3, _, _, s) ->
      String.Set.union_list [ Expr.used_vars e1; Expr.used_vars e2; Expr.used_vars e3; used_vars s ]
    | Seq ss -> String.Set.union_list (List.map ss ~f:used_vars)
    | Return (e, _) -> Expr.used_vars e

  let rec to_json = function
    | Skip -> `Assoc [ ("kind", `String "skip") ]
    | Assign (a, t, effs, e) ->
      `Assoc
        [
          ("kind", `String "assign");
          ("lhs", Assignable.to_json a);
          ("type", Type.to_json t);
          ("effs", Effect.Set.to_json effs);
          ("expr", Expr.to_json e);
        ]
    | If (e, effs, s1, s2) ->
      `Assoc
        [
          ("kind", `String "if");
          ("guard", Expr.to_json e);
          ("effs", Effect.Set.to_json effs);
          ("true", to_json s1);
          ("false", to_json s2);
        ]
    | While (e, effs, s) ->
      `Assoc
        [
          ("kind", `String "while");
          ("guard", Expr.to_json e);
          ("effs", Effect.Set.to_json effs);
          ("body", to_json s);
        ]
    | For (x, e1, e2, e3, effs, s) ->
      `Assoc
        [
          ("kind", `String "for");
          ("name", `String x);
          ("start", Expr.to_json e1);
          ("end", Expr.to_json e2);
          ("step", Expr.to_json e3);
          ("effs", Effect.Set.to_json effs);
          ("body", to_json s);
        ]
    | CFor (x, e1, e2, e3, effs, acc_fs, s) ->
      `Assoc
        [
          ("kind", `String "cfor");
          ("name", `String x);
          ("start", Expr.to_json e1);
          ("end", Expr.to_json e2);
          ("step", Expr.to_json e3);
          ("effs", Effect.Set.to_json effs);
          ("acc_fs", `List (List.map acc_fs ~f:(fun (acc, f) -> `List [ `String acc; `String f ])));
          ("body", to_json s);
        ]
    | Seq ss -> `Assoc [ ("kind", `String "seq"); ("body", `List (List.map ss ~f:to_json)) ]
    | Return (e, effs) ->
      `Assoc
        [ ("kind", `String "return"); ("expr", Expr.to_json e); ("effs", Effect.Set.to_json effs) ]
end

module Prog = struct
  type t = Stmt.t list [@@deriving compare, equal, sexp_of]

  let to_json t = `Assoc [ ("kind", `String "seq"); ("body", `List (List.map t ~f:Stmt.to_json)) ]
end
