open! Core
open! Ast

let () =
  let lexbuf = Lexing.from_channel In_channel.stdin in
  try
    let prog = Parser.prog Lexer.read lexbuf in
    Typechecker.check prog |> ok_exn;
    Interpreter.eval prog |> ok_exn
  with
  | Parser.Error ->
    let Lexing.{ pos_lnum; pos_cnum; pos_bol; _ } = lexbuf.lex_curr_p in
    print_s [%message "syntax error" ~line:(pos_lnum : int) ~col:(pos_cnum - pos_bol : int)]
