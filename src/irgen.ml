(* 
  IR gen, once again, shamelessly copied from MicroC as jumping off point.

  IR generation: translate takes a semantically checked AST and
  produces LLVM IR

  LLVM tutorial: Make sure to read the OCaml version of the tutorial

  http://llvm.org/docs/tutorial/index.html

  Detailed documentation on the OCaml LLVM library:

  http://llvm.moe/
  http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

(* translate : Sast.program -> Llvm.module *)
let translate (globals, functions) =
  let context    = L.global_context () in

  (* Create the LLVM compilation module into which
     we will generate code *)
  let the_module = L.create_module context "Comma" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context 
  and double_t   = L.double_type context
  and void_t     = L.void_type   context 
  and i64_t      = L.i64_type    context in

  (* Return the LLVM type for a Comma type *)
  let struct_matrix_type : L.lltype = L.named_struct_type context "Matrix" in
  let _ = L.struct_set_body struct_matrix_type
      [| i32_t ; i32_t ; L.pointer_type (L.pointer_type double_t) |] false in 
 

  (* Return the LLVM type for a MicroC type *)
  let rec ltype_of_typ = function
      A.Int       -> i32_t
    | A.Bool      -> i1_t
    | A.Double    -> double_t
    | A.Char      -> i8_t
    | A.Void      -> void_t
    | A.Array(t)  -> L.pointer_type (ltype_of_typ t)
    | A.Matrix -> L.pointer_type struct_matrix_type
  in

  let bind_typ = function
        SNoAssignBind(t,_) -> ltype_of_typ t
      | SAssignBind(t,_,_) -> ltype_of_typ t
  in

  (* Create a map of global variables after creating each *)
  let global_vars : L.llvalue StringMap.t =
    let global_var m bind = 
      let (t, n) = match bind with
       SNoAssignBind(ty,na) -> (ty, na)
      | SAssignBind(ty,na,_) -> (ty, na)
    in 
      let init = match t with
                  A.Double -> L.const_float_of_string double_t "0.0"
                  | A.Array(_) -> L.const_pointer_null (ltype_of_typ t)
                  | A.Matrix -> L.const_pointer_null (ltype_of_typ t)
                  | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in

  (* Declare standard library functions *)
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in 
  

  let initMatrix_t : L.lltype = 
    L.var_arg_function_type (L.pointer_type struct_matrix_type) [| i32_t ; i32_t ; L.pointer_type (L.pointer_type double_t) |] in 
  let initMatrix_func : L.llvalue = 
    L.declare_function "initMatrix" initMatrix_t the_module in

  let printMatrix_t : L.lltype = 
    L.var_arg_function_type void_t [| L.pointer_type struct_matrix_type |] in
  let printMatrix_func : L.llvalue = 
    L.declare_function "printMatrix" printMatrix_t the_module in
  
  let parseCSV_t : L.lltype =
    L.function_type (L.pointer_type struct_matrix_type) [| L.pointer_type i8_t |] in
  let parseCSV_func : L.llvalue =
    L.declare_function "parseCSV" parseCSV_t the_module in
  
  let outputCSV_t : L.lltype =
    L.function_type void_t [| (L.pointer_type struct_matrix_type) ; L.pointer_type i8_t |] in 
  let outputCSV_func : L.llvalue =
    L.declare_function "outputCSV" outputCSV_t the_module in

  let scalarMulti_t : L.lltype =
    L.function_type (L.pointer_type struct_matrix_type) [| double_t ; (L.pointer_type struct_matrix_type) |] in
  let scalarMulti_func : L.llvalue =
    L.declare_function "scalarMulti" scalarMulti_t the_module in

  let scalarDiv_t : L.lltype =
    L.function_type (L.pointer_type struct_matrix_type) [| double_t ; (L.pointer_type struct_matrix_type) |] in
  let scalarDiv_func : L.llvalue = 
    L.declare_function "scalarDiv" scalarDiv_t the_module in

  let subtractMatrix_t : L.lltype =
    L.function_type (L.pointer_type struct_matrix_type) [| (L.pointer_type struct_matrix_type) ; (L.pointer_type struct_matrix_type)|] in 
  let subtractMatrix_func : L.llvalue =
    L.declare_function "subtractMatrix" subtractMatrix_t the_module in

  let addMatrix_t : L.lltype = 
    L.function_type (L.pointer_type struct_matrix_type) [| (L.pointer_type struct_matrix_type) ; (L.pointer_type struct_matrix_type) |] in
  let addMatrix_func : L.llvalue =
    L.declare_function "addMatrix" addMatrix_t the_module in

  let dotProduct_t : L.lltype = 
    L.function_type (L.pointer_type struct_matrix_type) [| (L.pointer_type struct_matrix_type) ; (L.pointer_type struct_matrix_type)|] in 
  let dotProduct_func : L.llvalue = 
    L.declare_function "dotProduct" dotProduct_t the_module in

  (* cross product function *)

  let retrieveElement_t : L.lltype = 
    L.function_type double_t [| i32_t ; i32_t ; (L.pointer_type struct_matrix_type) |] in 
  let retrieveElement_func : L.llvalue = 
    L.declare_function "retrieveElement" retrieveElement_t the_module in 

  (* Define each function (arguments and return type) so we can
     call it even before we've created its body *)
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      and formal_types =
        Array.of_list (List.map bind_typ fdecl.sformals)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

  (* Return the value for a variable or formal argument.
     Check local names first, then global names *)
  let lookup n var_map = try StringMap.find n var_map;
    with Not_found -> StringMap.find n global_vars
  in

  (* Construct code for an expression; return its value *)
  let rec build_expr builder ((_, e) : sexpr) var_map = 
    match e with
      SIntLit i  -> L.const_int i32_t i
    | SBoolLit b  -> L.const_int i1_t (if b then 1 else 0)
    | SDoubLit d -> L.const_float_of_string double_t d
    | SNulLit -> L.const_int i32_t 0
    | SCharLit c -> L.const_int i8_t (int_of_char c)
    | SId s       -> L.build_load (lookup s var_map) s builder
    | SArrayLit(typ, a) -> let ret = match typ with (*Referenced SCIC project to get an idea on how to implement arrays and matrices*)
                              A.Int | A.Bool | A.Char -> let t = ltype_of_typ typ in
                                let build_one e = build_expr builder e var_map in
                                let arr = List.map build_one a in
                                let n = List.length a in
                                let ptr = L.build_array_malloc t (L.const_int t n) "" builder  in
                                ignore (List.fold_left (fun i elem ->
                                        let idx = L.const_int i32_t i in
                                        let eptr = L.build_gep ptr [|idx|] "" builder in
                                        let cptr = L.build_pointercast eptr 
                                            (L.pointer_type (L.type_of elem)) "" builder in
                                        let _ = (L.build_store elem cptr builder) in i + 1) 0 arr); ptr
                              | A.Double -> let t = ltype_of_typ typ in
                                let build_one e = build_expr builder e var_map in
                                let arr = List.map build_one a in
                                let n = List.length a in
                                let ptr = L.build_array_malloc t (L.const_int i64_t n) "" builder  in
                                ignore (List.fold_left (fun i elem ->
                                        let idx = L.const_int i32_t i in
                                        let eptr = L.build_gep ptr [|idx|] "" builder in
                                        let cptr = L.build_pointercast eptr 
                                            (L.pointer_type (L.type_of elem)) "" builder in
                                        let _ = (L.build_store elem cptr builder) in i + 1) 0 arr); ptr
                              | _ -> raise(Failure("Invalid array type"))
                            in ret
    | SMatrixLit(r, c, m) ->  let build_one e = build_expr builder e var_map in
                        let rows = List.map (List.map build_one) m in
                        let ptr = L.build_array_malloc (L.pointer_type double_t) (L.const_int i64_t r) "" builder in
                        ignore (List.fold_left (fun index ls -> 
                                let row_ptr = L.build_array_malloc (double_t) (L.const_int i64_t c) "" builder in 
                                ignore(List.fold_left (fun i elem ->
                                        let idx = L.const_int i32_t i in
                                        let eptr = L.build_gep row_ptr [|idx|] "" builder in
                                        let cptr = L.build_pointercast eptr 
                                                (L.pointer_type (L.type_of elem)) "" builder in
                                        let _ = (L.build_store elem cptr builder) in i + 1)
                                       0 ls);
                                let indxl = L.const_int i32_t index in 
                                let eptrr = L.build_gep ptr [| indxl |] "" builder in 
                                let cptrr = L.build_pointercast eptrr (L.pointer_type (L.pointer_type (double_t))) "" builder in 
                                let _ = (L.build_store row_ptr cptrr builder)
                                in index + 1)
                                0 rows);  
                       L.build_call initMatrix_func [| L.const_int i32_t r ; L.const_int i32_t c ; ptr |] "initMatrix" builder 
    | SAssign(s, e) -> let e' = build_expr builder e var_map in
      ignore(L.build_store e' (lookup s var_map) builder); e'
    | SBinop ((t1, e1), op, (t2, e2)) when t1 == A.Double ->
      let e1' = build_expr builder (t1, e1) var_map
      and e2' = build_expr builder (t1, e2) var_map in
      (match op with
         A.Add     -> L.build_fadd
       | A.Sub     -> L.build_fsub
       | A.And     -> L.build_and
       | A.Or      -> L.build_or
       | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
       | A.Neq     -> L.build_fcmp L.Fcmp.One
       | A.Less    -> L.build_fcmp L.Fcmp.Olt
       | A.Multiply -> L.build_fmul
       | A.Divide  -> L.build_fdiv
       | A.Great   -> L.build_fcmp L.Fcmp.Ogt
       | A.LessEqual -> L.build_fcmp L.Fcmp.Ole
       | A.GreatEqual -> L.build_fcmp L.Fcmp.Oge
       | _ -> raise(Failure("Illegal operation on double type"))
      ) e1' e2' "tmp" builder
    | SBinop ((t1, e1), op, (t2, e2)) ->
      let e1' = build_expr builder (t1, e1) var_map
      and e2' = build_expr builder (t1, e2) var_map in
      (match op with
         A.Add     -> L.build_add
       | A.Sub     -> L.build_sub
       | A.And     -> L.build_and
       | A.Or      -> L.build_or
       | A.Equal   -> L.build_icmp L.Icmp.Eq
       | A.Neq     -> L.build_icmp L.Icmp.Ne
       | A.Less    -> L.build_icmp L.Icmp.Slt
       | A.Multiply -> L.build_mul
       | A.Divide  -> L.build_sdiv
       | A.Mod     -> L.build_srem
       | A.Great   -> L.build_icmp L.Icmp.Sgt
       | A.LessEqual -> L.build_icmp L.Icmp.Sle
       | A.GreatEqual -> L.build_icmp L.Icmp.Sge
      ) e1' e2' "tmp" builder

           (* Evaluate standard library function calls *) 
    | SCall ("print", [e]) ->
      let print_it e =
        let e' = build_expr builder e var_map in 
        let (typ, _) = e in
        match typ with
          A.Array(typ) -> if typ = A.Char then L.build_call printf_func [| (L.build_global_stringptr "%s\n" "fmt" builder); e' |] "printf" builder
                              else raise(Failure("Unprintable argument given to print function"))
          | A.Int -> L.build_call printf_func [| (L.build_global_stringptr "%d\n" "fmt" builder); e' |] "printf" builder
          | A.Bool -> L.build_call printf_func [| (L.build_global_stringptr "%d\n" "fmt" builder); e' |] "printf" builder
          | A.Char -> L.build_call printf_func [| (L.build_global_stringptr "%c\n" "fmt" builder); e' |] "printf" builder
          | A.Double -> L.build_call printf_func [| (L.build_global_stringptr "%g\n" "fmt" builder); e' |] "printf" builder
          | _ -> raise(Failure("Unprintable argument given to print function")) in
      print_it e
    | SCall ("printMatrix", [e]) ->
      L.build_call printMatrix_func [| (build_expr builder e var_map) |] "" builder 
    
    | SCall ("parseCSV", [e]) -> 
      L.build_call parseCSV_func [| (build_expr builder e var_map) |] "parseCSV" builder
    | SCall ("outputCSV", [e1 ; e2]) -> 
      L.build_call outputCSV_func [| (build_expr builder e1 var_map) ; (build_expr builder e2 var_map) |] "" builder
    | SCall ("scalarMulti", [e1 ; e2]) ->
      L.build_call scalarMulti_func [| (build_expr builder e1 var_map) ; (build_expr builder e2 var_map) |] "scalarMulti" builder
    | SCall ("scalarDiv", [e1 ; e2]) ->
      L.build_call scalarDiv_func [| (build_expr builder e1 var_map) ; (build_expr builder e2 var_map) |] "scalarDiv" builder
    | SCall ("subtractMatrix", [e1 ; e2]) ->
      L.build_call subtractMatrix_func [| (build_expr builder e1 var_map) ; (build_expr builder e2 var_map) |] "subtractMatrix" builder
    | SCall ("addMatrix", [e1 ; e2]) ->
      L.build_call addMatrix_func [| (build_expr builder e1 var_map) ; (build_expr builder e2 var_map) |] "addMatrix" builder
    | SCall ("dotProduct", [e1 ; e2]) -> 
      L.build_call dotProduct_func [| (build_expr builder e1 var_map) ; (build_expr builder e2 var_map) |] "dotProduct" builder
    (* Cross product *)
    | SCall ("retrieveElement", [e1 ; e2 ; e3]) ->
      L.build_call retrieveElement_func [| (build_expr builder e1 var_map) ; (build_expr builder e2 var_map) ; (build_expr builder e3 var_map) |] "retrieveElement" builder
    
    | SCall (f, args) ->
      let (fdef, _) = StringMap.find f function_decls in
      let llargs = List.rev (List.map (fun e -> build_expr builder e var_map) (List.rev args)) in
      let result = f ^ "_result" in
      L.build_call fdef (Array.of_list llargs) result builder
    | SLambda (typ, arg, el, target) ->
      let new_local = L.build_alloca (ltype_of_typ typ) arg builder in
      let new_vars = StringMap.add arg new_local var_map in 
      ignore(build_expr builder (typ, (SAssign(arg, target))) new_vars);
      ignore(List.map (fun e -> build_expr builder e new_vars) el);
      build_expr builder (typ, SId(arg)) new_vars
  in

  (*Evaluate and assign the global variables*)
  let assign_global m bind =
        match bind with
          SNoAssignBind(t,n) -> m
        | SAssignBind(_,n,e) -> let (the_function, _) = StringMap.find "main" function_decls in
                                let temp_builder = L.builder_at_end context (L.entry_block the_function) in
                                let e' = build_expr temp_builder e global_vars in
                                ignore(L.build_store e' (lookup n m) temp_builder); m
  in
  ignore(List.fold_left assign_global global_vars globals);
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

        (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m bind p =
        match bind with
          SNoAssignBind(t,n) -> L.set_value_name n p;
                                 let local = L.build_alloca (ltype_of_typ t) n builder in
                                 ignore (L.build_store p local builder);
                                 StringMap.add n local m
        | SAssignBind(_,_,_) -> raise(Failure("Illegal assignment in function arguments"))

      (* Allocate space for any locally declared variables and add the
       * resulting registers to our map. Also, evaluate and add assigned local variables*)
      and add_local m bind =
        match bind with
          SNoAssignBind(t,n) -> let local_var = L.build_alloca (ltype_of_typ t) n builder
                                 in StringMap.add n local_var m
        | SAssignBind(t,n,e) -> let local_var = L.build_alloca (ltype_of_typ t) n builder
                                 in ignore(L.build_store (build_expr builder e m) local_var builder); 
                                 StringMap.add n local_var m
      in

      let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.slocals
    in

    (* LLVM insists each basic block end with exactly one "terminator"
       instruction that transfers control.  This function runs "instr builder"
       if the current block does not already have a terminator.  Used,
       e.g., to handle the "fall off the end of the function" case. *)
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
        Some _ -> ()
      | None -> ignore (instr builder) in

    (* Build the code for the given statement; return the builder for
       the statement's successor (i.e., the next instruction will be built
       after the one generated by this call) *)
    let rec build_stmt builder = function
        SBlock sl -> List.fold_left build_stmt builder sl
      | SExpr e -> ignore(build_expr builder e local_vars); builder
      | SReturn e -> ignore(L.build_ret (build_expr builder e local_vars) builder); builder
      | SIf (predicate, if_stmt, else_stmt) ->
        let bool_val = build_expr builder predicate local_vars in

        let if_bb = L.append_block context "then" the_function in
        ignore (build_stmt (L.builder_at_end context if_bb) if_stmt);
        let else_bb = L.append_block context "else" the_function in
        ignore (build_stmt (L.builder_at_end context else_bb) else_stmt);

        let end_bb = L.append_block context "if_end" the_function in
        let build_br_end = L.build_br end_bb in (* partial function *)
        add_terminal (L.builder_at_end context if_bb) build_br_end;
        add_terminal (L.builder_at_end context else_bb) build_br_end;

        ignore(L.build_cond_br bool_val if_bb else_bb builder);
        L.builder_at_end context end_bb

      | SWhile (predicate, body) ->
        let while_bb = L.append_block context "while" the_function in
        let build_br_while = L.build_br while_bb in (* partial function *)
        ignore (build_br_while builder);
        let while_builder = L.builder_at_end context while_bb in
        let bool_val = build_expr while_builder predicate local_vars in

        let body_bb = L.append_block context "while_body" the_function in
        add_terminal (build_stmt (L.builder_at_end context body_bb) body) build_br_while;

        let end_bb = L.append_block context "while_end" the_function in

        ignore(L.build_cond_br bool_val body_bb end_bb while_builder);
        L.builder_at_end context end_bb

      (*Build a while statement from a for statement*)
      | SFor (predicate, increment, body) ->
        build_stmt builder (SBlock [SWhile (predicate, SBlock[body; SExpr increment])])

    in
    (* Build the code for each statement in the function *)
    let func_builder = build_stmt builder (SBlock fdecl.sbody) in

    (* Add a return if the last block falls off the end *)
    add_terminal func_builder (L.build_ret (L.const_int i32_t 0))

  in
  List.iter build_function_body functions;
  the_module 
