open Lexing
open Parsing
type info = Lexing.position * Lexing.position

type logical_op = 
    | OR | AND | EQEQ | NE | LT | GT | LE | GE
type bitwise_op = 
    | ORB | XOR | ANDB | LEFT | RIGHT 
type arith_op =
    | ADD | SUB | DIV | MODULO | TIMES
type assign_op =
    | BitAssign of bitwise_op
    | ArithAssign of arith_op
    | EQ

type binary_op =
    | Logic of logical_op
    | Arith of arith_op
    | Bitwise of bitwise_op
    | Assign of assign_op
    | PTR | DOT 

type unary_op =
    | NOT | NOTB | ADDR | DEREF | MINUS
    | POST_INC | POST_DEC | PRE_INC | PRE_DEC

type endianness = LITTLE | BIG | DEFAULT
type type_size = S8 | S16 | S32 | S64 | SSIZE

type scalar_type = {base_type:base_type; endianness:endianness}
and base_type =
    | Signed of type_size
    | Unsigned of type_size
    | Float of type_size
    | Word of type_size
    | Bool of type_size
 
and node_expr = {pos:info; expr: expr; mutable dtype: dtype}
and expr =
    | Root of node_expr list
    | Return of node_expr
    | Decl of bool * string * dtype * node_expr
    | FunctionDecl of bool * string * string list * dtype * node_expr 
    | IfThenElse of node_expr * node_expr * node_expr
    | BinaryOperation of binary_op * node_expr * node_expr
    | UnaryOperation of unary_op * node_expr
    | While of node_expr * node_expr
    | FunctionCall of node_expr * node_expr list
    | Index of node_expr * node_expr
    | Block of node_expr list
    | IntLiteral of int64
    | FloatLiteral of float
    | StringLiteral of string
    | BoolLiteral of bool
    | Identifier of string
    | Nop
    (*| Null*)

and dtype =
    | Const of dtype
    | Pointer of dtype
    | StaticArray of dtype * int64
    | DynamicArray of dtype
    | FunctionType of dtype * dtype list
    | Scalar of scalar_type
    | Gen of int
    | Void
    | Undefined
    
let create_type_node info dtype = dtype
let create_expr_node info expr = {pos=info; expr=expr; dtype = Undefined}

let logical_op_str = function
    | OR    -> "||"
    | AND   -> "&&"
    | EQEQ  -> "=="
    | NE    -> "!="
    | LT    -> "<"
    | GT    -> ">"
    | LE    -> "<="
    | GE    -> ">="

let arith_op_str = function
    | ADD   -> "+"
    | SUB   -> "-"
    | DIV   -> "/"
    | MODULO-> "%"
    | TIMES -> "*"

let bitwise_op_str = function
    | ORB   -> "|"
    | XOR   -> "^"
    | ANDB  -> "&"
    | LEFT  -> "<<"
    | RIGHT -> ">>"
   
let assign_op_str = function
    | EQ -> "="
    | ArithAssign op -> "=" ^ (arith_op_str op)
    | BitAssign op -> "=" ^ (bitwise_op_str op)

let binary_op_str = function
    | Logic op -> logical_op_str op
    | Arith op -> arith_op_str op
    | Bitwise op -> bitwise_op_str op
    | Assign op -> assign_op_str op
    | PTR   -> "->"
    | DOT   -> "."

let unary_op_str = function
    | NOT      -> "!"
    | NOTB     -> "~"
    | ADDR      -> "&"
    | DEREF    -> "*"
    | MINUS    -> "-"
    | POST_INC -> "++ (post)" 
    | POST_DEC -> "-- (post)" 
    | PRE_INC  -> "++ (pre)"
    | PRE_DEC  -> "-- (pre)"
