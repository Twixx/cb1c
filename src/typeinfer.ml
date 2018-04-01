open Ast

exception InferError of info * string

module IdMap = Map.Make(String)

(*type environment = dtype IdMap.t*)

let def_scalar s =
    (Scalar { base_type = s; endianness = DEFAULT})
let current_type = ref 0

let new_gen_type () =
    incr current_type;
    Gen !current_type

let print_map m =
    let print_pair a b =
        print_string ("//(" ^ a ^ ", " ^ (Utils.pretty_type b) ^ ")\n")
    in
    IdMap.iter print_pair m

let rec annotate env ast =
    let raise_infer str = raise (InferError(ast.pos, str)) in
    let add_type t = ast.dtype <- t in
    let eannotate env ast =
        let _ = annotate env ast in ()
    in
    let add_env env id t =
        if IdMap.mem id env then
            raise_infer (id ^ " is already defined is this context")
        else
            IdMap.add id t env
    in
    match ast.expr with
    | Nop ->
            add_type Void;
            env
    | Root l ->
            let _ = List.fold_left annotate env l in
            add_type (new_gen_type ());
            (*print_map n;*)
            env
    | Return e ->
            eannotate env e;
            add_type (new_gen_type ());
            env
    | Decl (gen, name, dtype, init) ->
            eannotate env init;
            let t = (match dtype with
            | Undefined -> new_gen_type ()
            (* add func check *)
            | _ -> dtype) in
            add_type t;
            add_env env name t
    | FunctionDecl (gen, name, params, dtype, init) ->
            let rtype, ptypes = (match dtype with
            | Undefined ->
                    new_gen_type (), List.map (fun x -> new_gen_type ()) params
            | FunctionType(r, p_l) -> r, p_l
            | _ -> raise_infer "not a function type annotation")
            in
            let ftype = FunctionType(rtype, ptypes) in
            let new_env = (add_env env name ftype) in
            let inner_env =
                (try
                    List.fold_left2 add_env new_env params ptypes
                with Invalid_argument _ ->
                    raise_infer "fun types doesn't match the names number")
            in
            add_type ftype;
            eannotate inner_env init;
            new_env
    | IfThenElse(cond, bthen, belse) ->
            let new_env = annotate env cond in
            eannotate new_env bthen;
            eannotate new_env belse;
            add_type (new_gen_type ());
            env
    | BinaryOperation(op, e1, e2) ->
            eannotate env e1;
            eannotate env e2;
            add_type (new_gen_type ());
            env
    | UnaryOperation(op, e) ->
            eannotate env e;
            add_type (new_gen_type());
            env
    | While(cond, body) ->
            let new_env = annotate env cond in
            eannotate new_env body;
            add_type (new_gen_type());
            env
    | FunctionCall(e, e_list) ->
            eannotate env e;
            List.iter (eannotate env) e_list;
            add_type (new_gen_type());
            env
    | Index(e1, e2) ->
            eannotate env e1;
            eannotate env e2;
            add_type (new_gen_type());
            env
    | Block e_list ->
            let _ = List.fold_left annotate env e_list in
            add_type (new_gen_type());
            env
    | IntLiteral(i) ->
            add_type (def_scalar (Signed S32));
            env
    | FloatLiteral f ->
            add_type (def_scalar (Float S32));
            env
    | StringLiteral s ->
            add_type (StaticArray(def_scalar (Signed S32),
            Int64.of_int (String.length s)));
            env
    | BoolLiteral b ->
            add_type (def_scalar (Bool S32));
            env
    | Identifier id ->
            try 
                let t = IdMap.find id env in
                add_type t;
                env
            with Not_found ->
                raise_infer (id ^ " is not declared")

let rec collect_l l t = 
    List.fold_left (fun c e -> (collect e) @ c) [] l
    @ match l with
      | [] -> [(t, Void)]
      | _ -> [(t, (List.hd (List.rev l)).dtype)]

and collect_assign op e1 e2 t =
    let op_c = match op with
    | ArithAssign a -> collect_arith a e1 e2 t
    | BitAssign b -> collect_bitwise b e1 e2 t
    | EQ ->  (collect e2) @ (collect e1)
    in
    op_c @ [(e1, e2.dtype); (t, e1.dtype)]

and collect_arith op e1 e2 t =
    let int_t = def_scalar (Signed S32) in
    (collect e2) @ (collect e1) @
    [(e2, int_t); (e1, int_t);(t, int_t)]

and collect_logic op e1 e2 t =
    let int_t = def_scalar (Bool S32) in
    (collect e2) @ (collect e1) @
    [(e2, int_t); (e1, int_t);(t, int_t)]

and collect_bitwise op e1 e2 t =
    let int_t = def_scalar (Signed S32) in
    (collect e2) @ (collect e1) @
    [(e2, int_t); (e1, int_t);(t, int_t)]

and collect ast =
    let pos = ast.pos in
    let raise_infer str = raise (InferError(pos, str)) in
    let t = ast in
    match ast.expr with
    | Nop ->
            [(t, Void)]
    | Root l ->
            collect_l l t
    | Return e ->
            (collect e) @ [(t, Void)]
    | Decl (gen, name, dtype, init) ->
            (collect init) @ [(t, init.dtype)]
    | FunctionDecl (gen, name, params, dtype, init) ->
            (* FIXME Return somewhere return last problem*) 
            let ret_t = (match ast.dtype with
            | FunctionType(r, _) -> r
            | _ -> raise_infer "not a function") in
            (collect init) @ [(init, ret_t)]
    | IfThenElse(cond, bthen, belse) ->
            (collect cond) @ (collect bthen) @ (collect belse) @
            [(cond, def_scalar (Bool S32));
             (bthen, belse.dtype);
             (t, bthen.dtype)]
    | While(cond, body) ->
            (* Check for break *)
            (collect cond) @ (collect body) @
            [(cond, def_scalar (Bool S32)); (t, body.dtype)]
    | FunctionCall(e, p_l) ->
            (collect e) @ List.fold_left (fun c el -> c @ (collect el)) [] p_l @
            (match e.dtype with
            | FunctionType(ret_t, param_t_l) ->
                (try 
                    List.map2 (fun e1 e2 -> (e1, e2)) p_l param_t_l @
                    [(t, ret_t)]
                 with Invalid_argument _ ->
                        raise_infer "function parameters:number mismatch")
            | Gen _ ->
                    [(e, FunctionType(t.dtype, List.map (fun x -> x.dtype) p_l))]
            | _ -> raise_infer "not a function type or generic one")
    | BinaryOperation(op, e1, e2) ->
             (match op with 
             | Logic op -> collect_logic op e1 e2 t
             | Arith op -> collect_arith op e1 e2 t
             | Bitwise op -> collect_bitwise op e1 e2 t
             | Assign op -> collect_assign op e1 e2 t
             | PTR   ->  raise_infer "-> not supported"
             | DOT   -> raise_infer ". not supported")
    | UnaryOperation(op, e) ->
            raise_infer "unary_op not yet supported"
            (*match op with
            | NOT ->
            | NOTB ->
            | AMP ->
            | DEREF ->
            | MINUS ->
            | POST_INC | POST_DEC | PRE_INC | PRE_DEC*)
    | Index(e1, e2) ->
            raise_infer "index_op not yet supported"
            (*let e1_t = collect e1 in
            let e2_t = collect e2 in
            []*)
    | Block e_list ->
            collect_l e_list t
    | IntLiteral(i) -> []
    | FloatLiteral f -> []
    | StringLiteral s -> []
    | BoolLiteral b -> []
    | Identifier id -> []

let rec substitute x u t =
    let subs = substitute x u in
    match t with
    | Const t2 -> Const(subs t2)
    | Pointer t2 -> Pointer(subs t2)
    | StaticArray(t2, s) -> StaticArray(subs t2, s)
    | DynamicArray t2 -> DynamicArray(subs t2)
    | FunctionType(r_t, p_l) ->
        FunctionType(subs r_t, List.map subs p_l)
    | Gen id -> if id = x then u else t
    | Scalar _
    | Void
    | Undefined -> t

let apply subs t  =
  List.fold_right (fun (x, u) t -> substitute x u t) subs t

let rec unify constraints =
    match constraints with
    | [] -> []
    | (x, y) :: xs ->
          let t2 = unify xs in
          let t1 = unify_one (apply t2 x.dtype) (apply t2 y) x.pos in
            t1 @ t2

and unify_one t1 t2 pos =
    let raise_infer str = raise (InferError(pos, str)) in
    match t1, t2 with
    | Gen(x), z
    | z, Gen(x) -> [(x, z)]
    | Const c1, Const c2 -> unify_one c1 c2 pos
    | Pointer p1, Pointer p2 -> unify_one p1 p2 pos
    | StaticArray(b1, s1), StaticArray(b2, s2) ->
            if s1 = s2 then unify_one b1 b2 pos
            else raise_infer "different size array"
    | DynamicArray b1, DynamicArray b2 -> unify_one b1 b2 pos
    | Scalar s1, Scalar s2 ->
            if s1 <> s2 then raise_infer ("scalar type mismatch: " ^
            (Utils.pretty_scalar s1) ^ ", expected: " ^ (Utils.pretty_scalar s2))
            else []
    | FunctionType(r1, p1), FunctionType(r2, p2) ->
            let abs_type a =
                {pos=pos; dtype=a; expr=Nop}
            in
            unify ([(abs_type r1, r2)] @
            List.map2 (fun e1 e2 -> (abs_type e1, e2)) p1 p2)
    | Void, Void -> []
    | _ -> 
            raise_infer ("type mismatch: " ^ (Utils.pretty_type t1) ^
            " with " ^ (Utils.pretty_type t2))

let rec map_expr fexpr ftype ast =
    ast.dtype <- ftype ast.dtype;
    let recall = map_expr fexpr ftype in
    match ast.expr with
    | Root l ->
            List.iter recall l
    | Return e ->
            recall e
    | Decl (gen, name, dtype, init) ->
            recall init
    | FunctionDecl (gen, name, params, dtype, init) ->
            recall init
    | IfThenElse(cond, bthen, belse) ->
            recall cond;
            recall bthen;
            recall belse
    | BinaryOperation(op, e1, e2) ->
            recall e1;
            recall e2
    | UnaryOperation(op, e) ->
            recall e
    | While(cond, body) ->
            recall cond
    | FunctionCall(e, e_list) ->
            recall e;
            List.iter (recall) e_list
    | Index(e1, e2) ->
            recall e1;
            recall e2
    | Block e_list ->
            List.iter (recall) e_list
    | Nop
    | IntLiteral _
    | FloatLiteral _
    | StringLiteral _
    | BoolLiteral _ 
    | Identifier _ -> ()

let rec apply_expr subs ast =
    map_expr (apply_expr subs) (apply subs) ast

let infer ast =
    match ast.expr with
    | Root l ->
            let empty = IdMap.empty in
            let _ = annotate empty ast in
            let constraints = collect ast in
            let subs = unify constraints in
            (*let print_pair a b =
                print_string ("//(" ^ a ^ ", " ^ b ^ ")\n");
            in
            let rec print_c = function
            | [] -> ()
            | (a, b) :: t ->
                    print_pair (Utils.pretty_type a.dtype) (Utils.pretty_type b);
                    print_c t
            in print_c constraints;
            print_string "\n";*)
            apply_expr subs ast

    | _ -> raise (InferError(ast.pos,"ast to infer does not have a root"))


