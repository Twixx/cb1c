open Ast 
open Lexing

let idc = ref 0

let print_node label llabel p_id =
    idc := !idc + 1;
    print_string "N_";
    print_int !idc;
    print_string " [label=\"";
    print_string label;
    print_string "\" ];\nN_";
    print_int p_id;
    print_string " -- N_";
    print_int !idc;
    print_string " [label=\"";
    print_string llabel;
    print_string "\" ];\n";
    !idc

let rec print_type p_id llabel dtype =
    match dtype with 
    | Const e ->
            let current = print_node "Const" llabel p_id in
            print_type current "" e
    | Pointer e ->
            let current = print_node "Pointer" llabel p_id in
            print_type current "" e
    | StaticArray(e, s) ->
            let str = Int64.to_string s in
            let current = print_node ("Stat. Array: " ^ str) llabel p_id in
            print_type current "" e
    | DynamicArray e ->
            let current = print_node "Array" llabel p_id in
            print_type current "" e
    | FunctionType(e, e_list) ->
            let current = print_node "Function type" llabel p_id in
            print_type current "ret" e;
            List.iter (print_type current "par") e_list
    | Scalar s ->
            let str = Utils.pretty_scalar s in
            let _ = print_node str llabel p_id in ()
    | Gen i -> 
            let _ = print_node ("'" ^ string_of_int i) llabel p_id in ()
    | Void ->
            let _ = print_node "Void" llabel p_id in ()
    | Undefined ->
            let _ = print_node "Undefined" llabel p_id in ()

let rec print_ast p_id llabel ast =
    let type_str = Utils.pretty_type ast.dtype in
    let print_n str =
        print_node (type_str ^ " : " ^ str) llabel p_id in
    match ast.expr with
    | Return expr ->
            let current = print_n "Return" in
            print_ast current "" expr
    | Decl (gen, name, dtype, init) ->
            let s = "Decl" ^ (if gen then "(gen): " else ": ") ^ name in
            let current = print_n s in
            print_type current "type" dtype;
            print_ast current "init" init
    | FunctionDecl (gen, name, parms ,dtype, init) ->
            let name = name ^ "(" ^ (String.concat ", " parms) ^ ")" in
            let s = "Fun decl." ^ (if gen then "(gen):" else ":") ^ name in
            let current = print_n s in
            print_type current "type" dtype;
            print_ast current "init" init
    | IfThenElse(cond, bthen, belse) ->
            let current = print_n "IfThenElse" in
            print_ast current "if" cond;
            print_ast current "then" bthen;
            print_ast current "else" belse
    | BinaryOperation(op, e1, e2) ->
            let str = (binary_op_str op) in
            let current = print_n str in
            print_ast current "left" e1;
            print_ast current "right" e2
    | UnaryOperation(op, e) ->
            let current = print_n (unary_op_str op) in
            print_ast current "" e;
    | While(cond, body) ->
            let current = print_n "While" in
            print_ast current "cond" cond;
            print_ast current "body" body
    | FunctionCall(e, e_list) ->
            let current = print_n "Call" in
            print_ast current "func" e;
            List.iter (print_ast current "param") e_list
    | Index(e1, e2) ->
            let current = print_n "Index" in
            print_ast current "expr" e1;
            print_ast current "idx" e2
    | Block e_list ->
            let current = print_n "Block" in
            List.iter (print_ast current "") e_list;
    | IntLiteral(i) ->
            let str = Int64.to_string i in
            let _ = print_n ("Int (" ^ str ^ ")") in ()
    | FloatLiteral f ->
            let _ = print_n ("Float: " ^ (string_of_float f)) in ()
    | StringLiteral s ->
            let _ = print_n ("String: " ^ s) in ()
    | BoolLiteral b ->
            let _ = print_n ("Bool: " ^ (string_of_bool b)) in ()
    | Identifier id ->
            let _ = print_n ("Id: " ^ id) in ()
    (*| Null ->
            let _ = print_n "Null" in ()*)
    | Nop ->
            let _ = print_n "Nop" in ()
    | Root e_list -> 
            let current = print_n "Root" in
            List.iter (print_ast current "") e_list

let print_graph ast =
    let type_str = Utils.pretty_type ast.dtype in
    let str = type_str ^ " : root" in
    match ast.expr with
    | Root e_list ->
        print_string ("graph ast {\n N_0 [label=\"" ^ str ^ "\"];\n");
        List.iter (print_ast 0 "") e_list;
        print_string "}";
        flush stdout
    | _ -> print_string "not a root"

