open! Core
open! Ast

let main = "main"

let eval_func funcs ({ name; args; ret_type; body } : Func.t) vars =
  let%bind ctx =
    (fun () ->
      Map.merge Library.vars vars ~f:(fun ~key -> function
        | `Left expr
        | `Right expr ->
          Some expr
        | `Both _ -> raise_s [%message "shadowed library variable" ~name:key]))
    |> Or_error.try_with
  in
  let _ = funcs in
  ()

let eval prog =
  let open Or_error.Let_syntax in
  let%bind funcs =
    (fun () ->
      Map.merge Library.funcs prog ~f:(fun ~key -> function
        | `Left func
        | `Right func ->
          Some func
        | `Both _ -> raise_s [%message "shadowed library function" ~name:key]))
    |> Or_error.try_with
  in
  match Map.find funcs main with
  | None -> Or_error.error_string "no main function to execute"
  | Some func -> eval_func funcs func Var.Map.empty
