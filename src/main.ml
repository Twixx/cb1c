open Dotgraph

let _ =
    try
        let ast = Compil.parse stdin in
        Typeinfer.infer ast;
        print_graph ast
    with
        | Lexer.ParseError s -> print_string ("Unexpected token " ^ s ^ "\n");
        | Lexer.LexError s -> print_string (s ^ "\n")
        | Typeinfer.InferError(pos, str) ->
                print_string ((Utils.print_info pos) ^ ":\n" ^ str ^ "\n")

