open! Core
open! Ast

let () =
  Command.basic
    ~summary:"typecheck and execute input program"
    (let%map_open.Command filename = anon ("filename" %: string)
     and optimize = flag "--optimize" no_arg ~doc:"whether to apply optimizations"
     and print_prog = flag "--print" no_arg ~doc:"whether to print the executed program"
     and count = flag "--count" no_arg ~doc:"whether to count the number of statements executed" in
     fun () ->
       In_channel.with_file filename ~f:(fun c_in ->
           let lexbuf = Lexing.from_channel c_in in
           try
             let prog = Parser.prog Lexer.read lexbuf in
             Typechecker.check prog |> ok_exn;
             let prog = (if optimize then Optimizer.optimize else Fn.id) prog in
             if print_prog then print_s [%message (prog : Stmt.t list)];
             Interpreter.eval ~count prog |> ok_exn
           with
           | Parser.Error ->
             let Lexing.{ pos_lnum; pos_cnum; pos_bol; _ } = lexbuf.lex_curr_p in
             print_s
               [%message "syntax error" ~line:(pos_lnum : int) ~col:(pos_cnum - pos_bol : int)]))
  |> Command.run
