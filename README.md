# cb1c
CB1 OCaml implementation

Status:
- Grammar is not fixed and will probably change.
- Syntax error messages are for weak people.
- It can only output the AST now.

Lexer/Parser:
- String escaping is not fully supported (only \" and \\)
- Characters litterals are not supported

Type inference:
- Unification algorithm seems to work
- Only few expressions are supported now like declarations, function calls, assignements.

Build:
- You need Ocaml, menhir and ocamlbuild 
> make
It generate a binary called main.native.
This program expect input on stdin and output a dot format AST.

Example of the current syntax (_ holds for infered type):
    _ main(argc, argv) = {
        _ a = argc;
        _ f(test, var) = {
            var += if (test) 1 else 0;
        };
        f(a, argv);
        a;
    };
    
Many things are broken like declaring a variable without initialization, etc.


