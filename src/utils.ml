open Ast
open Lexing

exception ConvertError

let scalar_type_of_string str =
    try
        let endian =
            match String.sub str ((String.length str)-3) 3 with
            | "_le" -> LITTLE
            | "_be" -> BIG
            | _ -> DEFAULT
        in
        let size =
            match String.get str 1 with
            | '8' -> S8
            | '1' -> S16
            | '3' -> S32
            | '6' -> S64
            | 's' -> SSIZE
            | _ -> raise ConvertError
        in
        let btype =
            match String.get str 0 with 
            | 'u' -> Unsigned size
            | 's' -> Signed size
            | 'w' -> Word size
            | 'b' -> Bool size
            | 'f' -> Float size
            | _ -> raise ConvertError
        in {endianness=endian; base_type=btype}
    with Invalid_argument _ -> raise ConvertError

let pretty_scalar sc =
    let endian =
    match sc.endianness with
    | DEFAULT -> "de"
    | BIG -> "be"
    | LITTLE -> "le"
    in
    let t,s =
    match sc.base_type with
    | Signed s -> "s", s
    | Unsigned s -> "u", s
    | Bool s -> "b", s
    | Word s -> "w", s
    | Float s -> "f", s
    in
    let size =
    match s with 
    | S8 -> "8"
    | S16 -> "16"
    | S32 -> "32"
    | S64 -> "64"
    | SSIZE -> "size"
    (*| SMALLEST _ -> "small"*)
    in
    (t ^ size ^ endian)

let rec pretty_type t =
    match t with
    | Const e ->
            "cst(" ^ pretty_type e ^ ")"
    | Pointer e ->
            "ptr(" ^ pretty_type e ^ ")"
    | StaticArray(e, s) ->
            "[#" ^ Int64.to_string s ^ " of " ^ pretty_type e ^ "]"
    | DynamicArray e ->
            "[" ^ pretty_type e ^ "]"
    | FunctionType(e, e_list) ->
             "(" ^ (List.fold_left (fun n s -> n ^ (pretty_type s) ^ " -> ") "" e_list)
             ^ (pretty_type e) ^ ")"
    | Scalar s ->
            pretty_scalar s 
    | Gen i -> 
            "'" ^ (string_of_int i)
    | Void ->
            "void"
    | Undefined ->
            "undef."

let create_int_literal str base =
    try
        if base = 8 then Int64.of_string ("0o" ^ str)
        else Int64.of_string str
    with Failure _ -> raise ConvertError

let print_info inf = 
    let b, e = inf in
    if b.pos_lnum = e.pos_lnum then
        "Line " ^ string_of_int b.pos_lnum ^
         ", characters " ^ string_of_int (b.pos_cnum - b.pos_bol + 1) ^
         "-" ^ string_of_int (e.pos_cnum - e.pos_bol)
    else
        "Line " ^ string_of_int b.pos_lnum ^
         "character " ^ string_of_int (b.pos_cnum - b.pos_bol + 1) ^
         " to line " ^ string_of_int e.pos_lnum ^
         "character" ^ string_of_int (e.pos_cnum - e.pos_bol)
