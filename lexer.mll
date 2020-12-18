{
open Lexing
open Parser

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos; pos_lnum = pos.pos_lnum + 1 }
}

rule read =
  parse
  | [' ' '\t']+
    { read lexbuf }
  | '\r' | '\n' | "\r\n"
    { next_line lexbuf; read lexbuf }
  | "unit"
    { UNIT }
  | "int"
    { INT }
  | "bool"
    { BOOL }
  | "output"
    { OUTPUT }
  | "read"
    { READ }
  | "write"
    { WRITE }
  | "if"
    { IF }
  | "else"
    { ELSE }
  | "while"
    { WHILE }
  | "return"
    { RETURN }
  | '\\'
    { LAMBDA }
  | '-'? ['0'-'9']+
    { NUM (Base.Int.of_string (Lexing.lexeme lexbuf)) }
  | "true"
    { TRUE }
  | "false"
    { FALSE }
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '\'']*
    { ID (Lexing.lexeme lexbuf) }
  | '.'
    { DOT }
  | ':'
    { COLON }
  | ';'
    { SEMICOLON }
  | "->"
    { ARROW }
  | '!'
    { BANG }
  | '('
    { LPAREN }
  | ')'
    { RPAREN }
  | '{'
    { LBRACE }
  | '}'
    { RBRACE }
  | '='
    { ASSIGN }
  | _
    { Core.failwithf "syntax error at lexeme: %s" (Lexing.lexeme lexbuf) () }
  | eof
    { EOF }
