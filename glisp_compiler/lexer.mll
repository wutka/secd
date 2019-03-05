{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
        lexbuf.lex_curr_p <-
            { pos with pos_bol = lexbuf.lex_curr_pos;
              pos_lnum = pos.pos_lnum+1
            }
}

let int = '-'?['0'-'9']['0'-'9']*

let digit=['0'-'9']
let white=[' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment= ';' [^ '\n' '\r']* newline
let id=['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '_' '-' '0'-'9']*

rule read =
    parse
    | white {read lexbuf}
    | comment {next_line lexbuf; read lexbuf}
    | newline {next_line lexbuf; read lexbuf }
    | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
    | "defun" { DEFUN }
    | "defun-tail" { DEFUN_TAIL }
    | "defconst" { DEFCONST }
    | "let"   { LET }
    | "atom?" { ATOMP }
    | "cons"   { CONS }
    | "cdr"   { CDR }
    | "car"   { CAR }
    | "if"   { IF }
    | "nil"   { NIL }
    | "'"   { QUOTE }
    | "list"   { LIST }
    | "recur"   { RECUR }
    | "begin"   { BEGIN }
    | "lambda"   { LAMBDA }
    | "break"   { BREAK }
    | "t"   { T }
    | "(" { LPAREN }
    | ")" { RPAREN }
    | ">=" { GREATER_EQUAL }
    | ">" { GREATER }
    | "<=" { LESS_EQUAL }
    | "<" { LESS }
    | "=" { EQUAL }
    | "/=" { NOT_EQUAL }
    | "+" { PLUS }
    | "or" { PLUS }
    | "-" { MINUS }
    | "*" { TIMES }
    | "and" { TIMES }
    | "/" { DIVIDE }
    | id { ID (Lexing.lexeme lexbuf) }
    | _ { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
    | eof   {EOF}
