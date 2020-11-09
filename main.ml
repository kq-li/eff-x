open! Core

let () =
  print_endline "starting interpreter...";
  let rec aux () =
    printf "> %!";
    match In_channel.input_line In_channel.stdin with
    | None -> ()
    | Some line ->
      Lexing.from_string line
      |> Parser.expr_opt Lexer.read
      |> Option.iter ~f:(fun expr ->
             print_s [%message (expr : Ast.t)];
             let reduced = Interpreter.reduce expr in
             print_s [%message (reduced : Ast.t)];
             let result = Interpreter.eval expr in
             print_s [%message (result : int Or_error.t)])
      |> aux
  in
  aux ()
