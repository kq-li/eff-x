open! Core
open! Ast

let () =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  try Parser.prog Lexer.read lexbuf |> Interpreter.eval |> printf "%d\n" with
  | Parser.Error ->
    let Lexing.{ pos_lnum; pos_cnum; pos_bol; _ } = lexbuf.lex_curr_p in
    print_s [%message "syntax error" ~line:(pos_lnum : int) ~col:(pos_cnum - pos_bol : int)]
