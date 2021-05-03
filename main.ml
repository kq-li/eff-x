open! Core
open! Ast

let () =
  Command.basic
    ~summary:"parse and typecheck input program, outputting as json"
    (let%map_open.Command filename = anon ("filename" %: string)
     and optimize = flag "--optimize" no_arg ~doc:"whether to apply optimizations" in
     fun () ->
       In_channel.with_file filename ~f:(fun c_in ->
           let lexbuf = Lexing.from_channel c_in in
           try
             let prog = Parser.prog Lexer.read lexbuf in
             Typechecker.check prog |> ok_exn;
             let _ = optimize in
             prog
             (* |> (if optimize then Optimizer.optimize else Fn.id) *)
             |> Prog.to_json
             |> Yojson.Basic.pretty_to_string
             |> print_endline
           with
           | Parser.Error ->
             let Lexing.{ pos_lnum; pos_cnum; pos_bol; _ } = lexbuf.lex_curr_p in
             print_s
               [%message "syntax error" ~line:(pos_lnum : int) ~col:(pos_cnum - pos_bol : int)]))
  |> Command.run
