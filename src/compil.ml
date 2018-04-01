open Parsing
open Lexer

let parse channel =
    let lexbuf = Lexing.from_channel channel in
    try
        (*Parsing.set_trace true;*)
        Parser.program Lexer.token lexbuf
    with
    | Parser.Error -> raise (parse_error lexbuf)
