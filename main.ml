open! Core
open! Ast

let () =
  print_endline "starting interpreter...";
  let rec aux () =
    printf "> %!";
    match In_channel.input_line In_channel.stdin with
    | None -> ()
    | Some line ->
      let lexbuf = Lexing.from_string line in
      ( try
          Parser.expr_opt Lexer.read lexbuf
          |> Option.iter ~f:(fun expr ->
                 print_s [%message (expr : Expr.t)];
                 let typ = Expr.check_type expr in
                 print_s [%message (typ : Type.t Or_error.t)];
                 let reduced = Interpreter.reduce expr in
                 print_s [%message (reduced : Expr.t)];
                 let result = Interpreter.eval expr in
                 print_s [%message (result : Expr.t Or_error.t)])
          |> aux
        with
      | Parser.Error ->
        let Lexing.{ pos_lnum; pos_cnum; pos_bol; _ } = lexbuf.lex_curr_p in
        print_s [%message "syntax error" ~line:(pos_lnum : int) ~col:(pos_cnum - pos_bol : int)];
        aux () )
  in
  aux ()
