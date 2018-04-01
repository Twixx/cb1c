%{
    open Ast
    let node = create_expr_node
    let tnode = create_type_node
%}

%token<string> IDENTIFIER STRING_LITERAL
%token<float> FLOAT_LITERAL
%token<int64> INT_LITERAL

%token<Ast.scalar_type> SCALAR
%token COMMA OR AND TILD XOR AMP EQEQ ORB
%token PLUS MINUS TIMES SLASH MODULO NOT 
%token TRUE FALSE (*NULL*)
%token INC DEC LEFT RIGHT LE GE EQ NE LT GT
%token MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token LPAR RPAR LCBRA RCBRA LBRA RBRA SEMICOLON
%token CONST GEN UNDERSCORE
(*%token DOT UNKNOWN AS DO FOR  COLON  PTR*)
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN

%token	IF ELSE WHILE RETURN EOF
(*%token	CASE DEFAULT IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN*)

(* Precedences for dangling else *)
%nonassoc THEN
%nonassoc ELSE

%start program

%type <Ast.node_expr list> block_item_list

%type <Ast.dtype> const_typename array_typename (*variable_typename function_typename*) typename 
%type <Ast.node_expr> expression block loop_expression declaration program
%type <Ast.node_expr> jump_expression selection_expression block_item 
%type <Ast.node_expr> assignment_expression logical_and_expression logical_or_expression
%type <Ast.node_expr> inclusive_or_expression exclusive_or_expression and_expression equality_expression
%type <Ast.node_expr> relational_expression shift_expression additive_expression multiplicative_expression
%type <Ast.node_expr> cast_expression unary_expression postfix_expression primary_expression
%type <Ast.node_expr> variable_specifier function_specifier (*initialisation*)
%type <Ast.unary_op> unary_operator

%%

(* uselful parametrized rules *)
(* consume one "EXPR" expression or void with void priority *)
expr_or_void(EXPR):
| e = EXPR { e }
| (*empty*) { node ($startpos, $endpos) (Nop) } %prec THEN

(* program *)
program:
b = block_item_list EOF
{ node ($startpos, $endpos) (Root b) }

(* logical flow *)
%inline block_item_list:
| l = list(terminated(expr_or_void(block_item), SEMICOLON))
{ l }

block_item:
| i = expression
| i = declaration
{ i } 

expression:
| e = block
| e = selection_expression
| e = assignment_expression
| e = loop_expression
| e = jump_expression
{ e }

declaration:
| d = variable_specifier
| d = function_specifier
{ d }

block:
| LCBRA l = block_item_list RCBRA
{ node ($startpos, $endpos) (Block(l)) } 

selection_expression:
| IF LPAR cond = assignment_expression RPAR
  body = expression
  else_body = expr_or_void(preceded(ELSE, expression))
{ node ($startpos, $endpos) (IfThenElse(cond, body, else_body)) }

(*| SWITCH LPAR expression RPAR statement*)

loop_expression:
| WHILE LPAR cond = assignment_expression RPAR body = expression
{ node ($startpos, $endpos) (While(cond, body)) } 

(*| DO expression WHILE LPAR assignment_expression RPAR
| FOR LPAR assignment_expression SEMICOLON assignment_expression SEMICOLON assignment_expressionRPAR expression*)

jump_expression:
| RETURN e = expression SEMICOLON { node ($startpos, $endpos) (Return(e)) } 

(* assignment expression *)
primary_expression:
| id = IDENTIFIER { node ($startpos, $endpos) (Identifier(id)) } 
| l = INT_LITERAL { node ($startpos, $endpos) (IntLiteral(l)) } 
| l = FLOAT_LITERAL { node ($startpos, $endpos) (FloatLiteral(l)) } 
| l = STRING_LITERAL { node ($startpos, $endpos) (StringLiteral(l)) } 
| FALSE { node ($startpos, $endpos) (BoolLiteral(false)) } 
| TRUE { node ($startpos, $endpos) (BoolLiteral(true)) } 
(*| NULL { node ($startpos, $endpos) Null } *)
| LPAR e = assignment_expression RPAR { e } 

postfix_expression:
| e = primary_expression { e } 
| e = postfix_expression LBRA idx = expression RBRA
{ node ($startpos, $endpos) (Index(e, idx)) } 
| f = postfix_expression LPAR par = separated_list(COMMA, assignment_expression) RPAR
{ node ($startpos, $endpos) (FunctionCall(f, par)) } 
    (*| postfix_expression '.' IDENTIFIER
| postfix_expression PTR IDENTIFIER*)
| e = postfix_expression INC
{ node ($startpos, $endpos) (UnaryOperation(POST_INC, e)) } 
| e = postfix_expression DEC
{ node ($startpos, $endpos) (UnaryOperation(POST_DEC, e)) } 
    (*| LPAR type_name RPAR LCBRA initializer_list RCBRA
| LPAR type_name RPAR LCBRA initializer_list COMMA RCBRA*)

unary_expression:
| e = postfix_expression { e } 
| INC e = unary_expression
{ node ($startpos, $endpos) (UnaryOperation(PRE_INC, e)) } 
| DEC e = unary_expression
{ node ($startpos, $endpos) (UnaryOperation(PRE_DEC, e)) } 
| op = unary_operator e = cast_expression
{ node ($startpos, $endpos) (UnaryOperation(op, e)) } 
(*| unary_operator cast_expression { node $@ UnaryOperation($1,)}*)

unary_operator:
| AMP { ADDR } 
| TIMES { DEREF } 
| MINUS { MINUS } 
| TILD { NOTB } 
| NOT { NOT }

cast_expression:
| e = unary_expression { e } 
    (*| cast_expression AS typename { $1*)

%inline mult_op:
| TIMES { TIMES }
| SLASH { DIV }
| MODULO { MODULO }

multiplicative_expression:
| e = cast_expression { e } 
| l = multiplicative_expression op = mult_op r = cast_expression
{ node ($startpos, $endpos) (BinaryOperation(Arith op, l, r)) } 

%inline add_op:
| MINUS { SUB }
| PLUS { ADD }

additive_expression:
| e = multiplicative_expression { e } 
| l = additive_expression op = add_op r = multiplicative_expression
{ node ($startpos, $endpos) (BinaryOperation(Arith op, l, r)) } 

%inline sh_op:
| LEFT { RIGHT }
| RIGHT { RIGHT }

shift_expression:
| e = additive_expression { e } 
| l = shift_expression op = sh_op r = additive_expression
{ node ($startpos, $endpos) (BinaryOperation(Bitwise op, l, r)) } 

%inline comp_op:
| LT { LT }
| GT { GT }
| LE { LE }
| GE { GE }

relational_expression:
| e = shift_expression { e } 
| l = relational_expression op = comp_op r = shift_expression
{ node ($startpos, $endpos) (BinaryOperation(Logic op, l, r)) } 

%inline eq_op:
| EQEQ { EQEQ }
| NE { NE }

equality_expression:
| e = relational_expression { e } 
| l = equality_expression op = eq_op r = relational_expression
{ node ($startpos, $endpos) (BinaryOperation(Logic op, l, r)) } 

and_expression:
| e = equality_expression { e } 
| l = and_expression AMP r = equality_expression
{ node ($startpos, $endpos) (BinaryOperation(Bitwise ANDB, l, r)) } 

exclusive_or_expression:
| e = and_expression { e } 
| l = exclusive_or_expression XOR r = and_expression
{ node ($startpos, $endpos) (BinaryOperation(Bitwise XOR, l, r)) } 

inclusive_or_expression:
| e = exclusive_or_expression { e } 
| l = inclusive_or_expression ORB r = exclusive_or_expression
{ node ($startpos, $endpos) (BinaryOperation(Bitwise ORB, l, r)) } 

logical_and_expression:
| e = inclusive_or_expression { e } 
| l = logical_and_expression AND r = inclusive_or_expression
{ node ($startpos, $endpos) (BinaryOperation(Logic AND, l, r)) } 

logical_or_expression:
| e = logical_and_expression { e } 
| l = logical_or_expression OR r = logical_and_expression
{ node ($startpos, $endpos) (BinaryOperation(Logic OR, l, r)) } 

%inline assignment_operator:
| EQ { EQ } 
| MUL_ASSIGN { ArithAssign TIMES } 
| DIV_ASSIGN { ArithAssign DIV } 
| MOD_ASSIGN { ArithAssign MODULO } 
| ADD_ASSIGN { ArithAssign ADD } 
| SUB_ASSIGN { ArithAssign SUB } 
| LEFT_ASSIGN { BitAssign LEFT } 
| RIGHT_ASSIGN { BitAssign RIGHT } 
| AND_ASSIGN { BitAssign ANDB } 
| XOR_ASSIGN { BitAssign XOR } 
| OR_ASSIGN { BitAssign ORB } 

assignment_expression:
| e = logical_or_expression { e } 
| l = unary_expression op = assignment_operator r = expression
{ node ($startpos, $endpos) (BinaryOperation(Assign op, l, r)) } 


(* declarations *)
variable_specifier:
    gen = boption(GEN) 
    t = typename
    id = IDENTIFIER
    init = option(preceded(EQ, expression))
    { node ($startpos, $endpos) (Decl(gen, id, t, init)) } 

function_specifier:
    gen = boption(GEN)
    t = typename
    name = IDENTIFIER
    LPAR par = separated_list(COMMA, IDENTIFIER) RPAR
    init = option(preceded(EQ, expression))
    { node ($startpos, $endpos) (FunctionDecl(gen, name, par, t, init)) }

(* types*)
typename :
| UNDERSCORE {Undefined}
| t = const_typename { t } 
| t = typename CONST { tnode ($startpos, $endpos) (Const(t)) }(* pointer *)
| t = typename TIMES { tnode ($startpos, $endpos) (Pointer(t)) }(* pointer *)
| ret = typename LPAR par = separated_list(COMMA, typename) RPAR
{ tnode ($startpos, $endpos) (FunctionType(ret, par)) }(* fun *)
(*| IDENTIFIER { node $@ Typename($1)}*)

const_typename:
| t = array_typename { t } 
| CONST t = const_typename { tnode ($startpos, $endpos) (Const(t)) }(* const *)

array_typename:
| s = SCALAR { tnode ($startpos, $endpos) (Scalar s) } 
| LBRA t = typename RBRA{ tnode ($startpos, $endpos) (DynamicArray(t)) }(* array *)
| LBRA t = typename SEMICOLON s = INT_LITERAL RBRA
{ tnode ($startpos, $endpos) (StaticArray(t, s)) }(* array *)

%%

