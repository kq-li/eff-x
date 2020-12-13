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
      ( try () |> aux with
      | Parser.Error ->
        let Lexing.{ pos_lnum; pos_cnum; pos_bol; _ } = lexbuf.lex_curr_p in
        print_s [%message "syntax error" ~line:(pos_lnum : int) ~col:(pos_cnum - pos_bol : int)];
        aux () )
  in
  aux ()
