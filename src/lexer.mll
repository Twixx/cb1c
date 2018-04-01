{
        open Parser        (* The type token is defined in parser.mli *)
        open Lexing
        open Parsing

        exception ParseError of string
        exception LexError of string
        
        let error msg start finish  = 
            Printf.sprintf "(line %d: char %d..%d): %s" start.pos_lnum 
            (start.pos_cnum - start.pos_bol) (finish.pos_cnum - finish.pos_bol) msg

        let lex_error msg lexbuf = 
            raise ( LexError (error (msg ^ (lexeme lexbuf)) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)))

        let parse_error lexbuf =
            raise ( ParseError (error (lexeme lexbuf) (lexeme_start_p lexbuf) (lexeme_end_p lexbuf)))

        let newline lexbuf =
            let pos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <-
                { pos with pos_lnum = pos.pos_lnum + 1;
                   pos_bol = pos.pos_cnum }

        let comment_depth = ref 0
        let string_buffer = Buffer.create 20

        let create_float str lexbuf =
            try
                FLOAT_LITERAL (float_of_string str)
            with Failure _ ->
                lex_error "Literal overflow (over 64bits): " lexbuf

        let create_int str base lexbuf =
            try
                INT_LITERAL (Utils.create_int_literal str base)
            with Utils.ConvertError ->
                lex_error "Literal overflow (limited to 63 bits now): " lexbuf

}

let non_zero_digit  = ['1'-'9']
let digit           = ['0'-'9']
let octal_digit     = ['0'-'7']
let lowercase       = ['a'-'z']
let uppercase       = ['A'-'Z']
let hex_digit       = digit|['A'-'F']|['a'-'f']
let alpha           = uppercase|lowercase|'_'
let decimal_literal = non_zero_digit(digit|'_')*
let hex_literal     = "0x"'_'?(hex_digit|'_')*
let octal_literal   = '0''_'?(octal_digit|'_')*
let float_part      = '_'?(digit)(digit|'_')*
let noexp_float     = (decimal_literal|'0')'.'(float_part)?|(decimal_literal|'0')?'.'(float_part)
let exponent        = ('e'|'E')('+'|'-')?(digit|'_')+
let float_literal   = (noexp_float)(exponent)?

rule token = parse
| '\n'                 { newline lexbuf; token lexbuf}
| [' ' '\t' '\n']      { token lexbuf }     (* skip blanks *)
| "/*"                 { comment_depth := 1; comment lexbuf }
| '"'
    { Buffer.clear string_buffer;
      string lexbuf;
      STRING_LITERAL (Buffer.contents string_buffer) }
| "++"                 { INC          }
| "--"                 { DEC          }
| "=="                 { EQEQ         }
| '='                  { EQ           }
| "&&"                 { AND          }
| "||"                 { OR           }
| '|'                  { ORB          }
| "+="                 { ADD_ASSIGN   }
| "-="                 { SUB_ASSIGN   }
| "*="                 { MUL_ASSIGN   }
| "/="                 { DIV_ASSIGN   }
| "%="                 { MOD_ASSIGN   }
| "<<="                { LEFT_ASSIGN  }
| ">>="                { RIGHT_ASSIGN }
| "&="                 { AND_ASSIGN   }
| "^="                 { XOR_ASSIGN   }
| "|="                 { OR_ASSIGN    }
| '^'                  { XOR          }
| '&'                  { AMP         }
| "!="                 { NE           }
| "<<"                 { LEFT         }
| ">>"                 { RIGHT        }
| "<="                 { LE           }
| ">="                 { GE           }
| '<'                  { LT           }
| '>'                  { GT           }
| '+'                  { PLUS         }
| '-'                  { MINUS        }
| '*'                  { TIMES        }
| '/'                  { SLASH        }
| '%'                  { MODULO       }
| '!'                  { NOT          }
| '~'                  { TILD         }
| '('                  { LPAR         }
| ')'                  { RPAR         }
| '{'                  { LCBRA        }
| '}'                  { RCBRA        }
| '['                  { LBRA         }
| ']'                  { RBRA         }
| ';'                  { SEMICOLON    }
| ','                  { COMMA        }
| '_'                  { UNDERSCORE   }
| "return"             { RETURN       }
| "const"              { CONST        }
(*| "do"                 { DO           }
| "->"                 { PTR          }
| "as"                 { AS           } 
| ':'                  { COLON        }
| '.'                  { DOT          }
| "null"               { NULL     }
| "for"                { FOR          }*)
| "if"                 { IF           }
| "else"               { ELSE         }
| "while"              { WHILE        }
| "true"               { TRUE         }
| "false"              { FALSE        }
| "gen"                { GEN          }

| ("f32"|"f64"|(('b'|'u'|'s'|'w')('8'|"16"|"32"|"64"|"size")))('_'("le"|"be"))? as s {
    try
        SCALAR (Utils.scalar_type_of_string s);
    with Utils.ConvertError -> lex_error "invalid type" lexbuf
}

| alpha(alpha|digit)* as id {
    IDENTIFIER id
}

| float_literal as f {
    create_float f lexbuf
}

| decimal_literal as d {
    create_int d 10 lexbuf
}

| octal_literal as o {
    create_int o 8 lexbuf
}

| hex_literal as h {
    create_int h 16 lexbuf
}
| eof                 { EOF }
| _                   { lex_error "unexpected char: " lexbuf }

and comment = parse
"*/"                { decr comment_depth;
                        if !comment_depth = 0 then token lexbuf
                        else comment lexbuf }
| "/*"              { incr comment_depth; comment lexbuf}
| '\n'              { newline lexbuf; comment lexbuf}
| eof               { lex_error "non terminated comment" lexbuf }
| _                 { comment lexbuf }
        
and string = parse
   | '"'
    { () }
   | '\\' '\\'
    { Buffer.add_char string_buffer '\\';
      string lexbuf }
   | '\\' '"'
    { Buffer.add_char string_buffer '"';
      string lexbuf }
   | '\n' eof
    {lex_error "non terminated string" lexbuf}
   | _ as c
    { Buffer.add_char string_buffer c;
      string lexbuf }
