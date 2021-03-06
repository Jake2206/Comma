(* semantic checking *)

open Ast
open Sast
module StringMap = Map.Make(String)

let func_decls = ref StringMap.empty;;

let check (globals, functions) = 
    (* Check assignment (types match) *)
	let check_assign lvaluetype rvaluetype err =
		if lvaluetype = rvaluetype then lvaluetype else raise (Failure err) 
	in
	
	(* Check built-in functions *)
	let built_in_decls =
		let map = StringMap.empty in
		let map = 
			StringMap.add "print" {
				rtyp = Void;
				fname = "print";
				formals = [NoAssignBind(Array Char, "i")];
				locals = [];
				body = [];
			} map
		in
                let map = 
                        StringMap.add "printMatrix" {
                                rtyp = Void;
                                fname = "printMatrix";
                                formals = [NoAssignBind(Matrix, "Matrix")];
                                locals = [];
                                body = []; 

                        } map
                in
		let map = 
			StringMap.add "parseCSV" {
				rtyp = Matrix;
				fname = "parseCSV";
				formals = [NoAssignBind(Array Char, "input_filepath")]; 
				locals = []; 
				body = [];
			} map
		in
		let map = 
			StringMap.add "outputCSV" {
				rtyp = Void;
				fname = "outputCSV";
				formals = [NoAssignBind(Matrix, "matrix"); NoAssignBind(Array Char, "outputFilepath")];
				locals = []; 
				body = [];
			} map
		in
		let map =
			StringMap.add "scalarMulti" {
				rtyp = Matrix;
				fname = "scalarMulti";
				formals = [NoAssignBind(Double, "scalar"); NoAssignBind(Matrix, "base_matrix")];
				locals = []; 
				body = [];
			} map
		in
		let map = 
			StringMap.add "scalarDiv" {
				rtyp = Matrix;
				fname = "scalarDiv";
				formals = [NoAssignBind(Double, "scalar"); NoAssignBind(Matrix, "base_matrix")];  
				locals = []; 
				body = [];
			} map
		in
		let map =
			StringMap.add "subtractMatrix" {
				rtyp = Matrix;
				fname = "subtractMatrix";
				formals = [NoAssignBind(Matrix, "base_matrix"); NoAssignBind(Matrix, "sub_matrix")]; 
				locals = []; 
				body = [];
			} map
		in 
		let map = 
			StringMap.add "addMatrix" {
				rtyp = Matrix;
				fname = "addMatrix";
				formals = [NoAssignBind(Matrix, "base_matrix"); NoAssignBind(Matrix, "add_matrix")];
				locals = []; 
				body = [];
			} map
		in
		let map = 
			StringMap.add "dotProduct" {
				rtyp = Matrix;
				fname = "dotProduct";
				formals = [NoAssignBind(Matrix, "base_matrix"); NoAssignBind(Matrix, "dot_matrix")];
				locals = []; 
				body = [];
			} map
		in
		let map =
			StringMap.add "crossProduct" {
				rtyp = Matrix;
				fname = "crossProduct";
				formals = [NoAssignBind(Array Double, "base_vector"); NoAssignBind(Array Double, "cross_vector")];
				locals = []; 
				body = [];
			} map
		in
		let map = 
			StringMap.add "length" {
				rtyp = Int;
				fname = "length";
				formals = [NoAssignBind(Array Int, "array")];
				locals = []; 
				body = [];
			} map
		in
		let map =
			StringMap.add "dimension" {
				rtyp = Array Int;
				fname = "dimension";
				formals = [NoAssignBind(Matrix, "matrix")];
				locals = []; 
				body = [];
			} map
		in 
		let map = 
			StringMap.add "retrieveElement" {
				rtyp = Double;
				fname = "retrieveElement";
				formals = [NoAssignBind(Int, "row_index"); NoAssignBind(Int, "column_index"); NoAssignBind(Matrix, "matrix")];
				locals = []; 
				body = [];
			} map
		in map
	in
	
	(* Add function name to symbol table *)
	let add_func map fd =
		let build_in_err = "function " ^ fd.fname ^ " may not be defined"
		and dup_err = "duplicate function " ^ fd.fname
		and make_err er = raise (Failure er)
		and n = fd.fname
		in match fd with
			_ when StringMap.mem n built_in_decls -> make_err build_in_err
			| _ when StringMap.mem n map -> make_err dup_err
			| _ -> StringMap.add n fd map
	in 
	
	(* Collect all function names into one symbol table *)
	func_decls := List.fold_left add_func built_in_decls functions;
	
	(* Find function in table *)
	let find_func s =
		try StringMap.find s !func_decls
		with Not_found -> raise (Failure ("no such function declared: " ^ s))
	in
	
	(* Helper function to get expression type and derived type *)
	let derive_expr_in_context e symbol_list =
	    (* Variable table: keep track of type global, formal, local
			-> formal variables are arguments passed to a function *)
		let symbols =
					  List.fold_left
						(fun m x ->
							match x with
								AssignBind(t, n, _) -> StringMap.add n t m
								| NoAssignBind(t, n) -> StringMap.add n t m
						) StringMap.empty symbol_list
		in
		
		let type_of_identifier s symbols =
			try StringMap.find s symbols
			with Not_found ->raise (Failure ("undeclared symbol: " ^ s))
		in 			

        (* Evaluate an expression *)		
		let rec expr e symbols = match e with
			NulLit -> (Void, SNulLit)
			| IntLit l -> (Int, SIntLit l)
			| BoolLit l -> (Bool, SBoolLit l)
			| CharLit l -> (Char, SCharLit l)
			| DoubLit l -> (Double, SDoubLit l)
			| ArrayLit(t, a) -> let err rt ex = "Illegal array entry: " ^ 
									string_of_typ t ^ " = " ^ string_of_typ rt ^ " in " ^ 
									string_of_expr ex in
								let get_derived e = let (ty, e') = expr e symbols in ignore(check_assign t ty (err ty e)); (ty, e') in
								let entries = List.map get_derived a in
								(Array t, SArrayLit(t, entries))
			| MatrixLit(m) ->   let len = List.length (List.hd m) in
								let err rt ex = "Illegal matrix entry: " ^ 
									"expected double but found " ^ string_of_typ rt ^ " in " ^ 
									string_of_expr ex in
								let get_derived e = let (ty, e') = expr e symbols in ignore(check_assign Double ty (err ty e)); (ty, e') in
                                                                let row_num = List.length m in
								let get_single arr = let cur_len = List.length arr in
													if cur_len = len then
                                                                                                            
                                                                                                            List.map get_derived arr 
                                                                                                        else 
													    raise (Failure ("Illegal row length in matrix. Expected length " ^ string_of_int len ^ " got length " ^ string_of_int cur_len)) in
								let entries = List.map get_single m in
								(Matrix, SMatrixLit(row_num, len, entries))
			| Id l      -> (type_of_identifier l symbols, SId l)
			| Binop(e1, op, e2) -> 
					let (lt, e1derived) = expr e1 symbols 
					and (rt, e2derived) = expr e2 symbols in
					let same = lt = rt in
					let ty = match op with
						Add | Sub | Multiply | Divide | Mod when same && lt = Int  -> Int
						| Add | Sub | Multiply | Divide when same && lt = Double -> Double
						| Equal | Neq            when same 				-> Bool
						| Less | Great | LessEqual | GreatEqual 
												 when same && (lt = Int || lt = Double) -> Bool
						| And | Or when same && lt = Bool 				-> Bool
						| _ -> raise (Failure ("illegal binary operation"))
					in (ty, SBinop((lt, e1derived), op, (rt, e2derived)))
			| Assign(var, e) as ex -> 
					let lt = type_of_identifier var symbols
					and (rt, ederived) = expr e symbols in
					let err = "Illegal assignment: " ^ 
							string_of_typ lt ^ " = " ^ string_of_typ rt ^ " in " ^ 
							string_of_expr ex
					in (check_assign lt rt err, SAssign(var, (rt, ederived)))
			| Call(fname, args) as call ->
				let fd = find_func fname in
				let param_length = List.length fd.formals in
				if List.length args != param_length then
				  raise (Failure ("expecting " ^ string_of_int param_length ^
								  " arguments in " ^ string_of_expr call))
				else let check_call bind_arg e =
					   let bind_typ = 
							match bind_arg with
								AssignBind(_, _, _) -> raise (Failure ("illegal expression in function args"))
								| NoAssignBind(t, _) -> t
							in 
					   let (et, e') = expr e symbols in
					   let err = "Illegal argument found " ^ string_of_typ et ^
								 " expected " ^ string_of_typ bind_typ ^ " in " ^ string_of_expr e ^ " in function " ^ fname
					   in match fname with
					    "print" -> (et, e')
					    | _ -> (check_assign bind_typ et err, e') 
				  in
				  let args' = List.map2 check_call fd.formals args
				  in (fd.rtyp, SCall(fname, args'))
			| Lambda(typ, arg, el, target) ->
					let new_symbols = StringMap.add arg typ symbols
					in let one_ex e = expr e new_symbols
					in let el' = List.map one_ex el
					in let target' = expr target symbols
					in (typ, SLambda(typ, arg, el', target'))
		in expr e symbols
	in
	
	let check_binds (kind  : string) (binds : bind list) =
		(* Check variables bind to a real type (not null)
			And check that formals do not have expressions with declaration*)
		List.iter (function
			AssignBind(Void, _, _)  -> raise (Failure ("illegal bind: cannot be of type nul"))
			| NoAssignBind(Void, _) -> raise (Failure ("illegal bind: cannot be of type nul"))
			| AssignBind(_, _, _) when kind = "formal" -> raise (Failure ("illegal bind: cannot bind expression in function arguments"))
			| _ -> ()) binds;
		
		(* Check assigns on variables to make sure types are not mismatched *)
		List.iter (function
		    AssignBind(t, _, e) -> 
			    let (dt, _) = derive_expr_in_context e binds in
				if dt == t then
					()
				else
					if dt == Void then
						()
					else
						if string_of_typ t == string_of_typ dt then 
						() 
						else 
							raise (Failure ("Illegal bind: mismatched types: expected '" 
								^ string_of_typ t ^ "' got '" ^ string_of_typ dt 
								^ "' instead in expr: " ^ string_of_expr e))
			| _ -> ()) binds;

		(* Check no two variables have same name within same scope. *)
		let compare_binds x y =
			match (x, y) with
				(AssignBind(_,n1,_), AssignBind(_,n2,_)) when n1 = n2 -> true
				| (AssignBind(_,n1,_), NoAssignBind(_,n2)) when n1 = n2 -> true
			    | (NoAssignBind(_,n1), AssignBind(_,n2,_)) when n1 = n2 -> true
				| (NoAssignBind(_, n1), NoAssignBind(_, n2)) when n1 = n2 -> true
				| (_,_) -> false
		in 
		let rec dups = function 
			[] -> ()
			| (x :: y :: _) when (compare_binds x y) -> raise (Failure ("duplicate declaration"))
			| _ :: t -> dups t
		in 
		let sort_bind_list = 
			List.sort (
				fun x y -> 
					match (x, y) with
						(AssignBind(_,n1,_), AssignBind(_,n2,_))     -> compare n1 n2
						| (AssignBind(_,n1,_), NoAssignBind(_,n2))   -> compare n1 n2
						| (NoAssignBind(_,n1), AssignBind(_,n2,_))   -> compare n1 n2
						| (NoAssignBind(_, n1), NoAssignBind(_, n2)) -> compare n1 n2
			) binds
		in dups (sort_bind_list);
	in 
	
	(* Check global variables *)
	check_binds "globals" globals;
	
	(* Ensure a main function exists *)
	let _ = find_func "main" in
	
	(* ------------------Check function declarations -------------------------- *)
	
	let check_function func =
		check_binds "formal" func.formals;
		check_binds "local" (globals@func.formals@func.locals);
		
		(*Helper function to convert binds to Sbinds*)
		let store_binds binds = let get_one = function 
			| AssignBind(t,n,e) -> SAssignBind(t,n,(derive_expr_in_context e binds))
			| NoAssignBind(t,n) -> SNoAssignBind(t,n)
			in
			List.map get_one binds
		in

		(*Convert binds to Sbind types*)
		let sformals = store_binds (func.formals)
		in
		let slocals =  store_binds (globals@func.formals@func.locals)
		in 

	    let context = ( globals @ func.formals @ func.locals )	
		in
		
		(* Check expression returns a boolean *)
		let check_bool_expr e = 
			let (typder, exprder) = derive_expr_in_context e context
			and err = "Expected Boolean expression"
			in if typder != Bool then raise (Failure err) else (typder, exprder)
		in
		(* Check statement *)
		let rec check_stmt = function
			Expr e -> SExpr (derive_expr_in_context e context)
			| If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
			| For(e2, e3, st) -> SFor(check_bool_expr e2, derive_expr_in_context e3 context, check_stmt st)
			| While (p, s) -> SWhile(check_bool_expr p, check_stmt s)
			| Return e -> let (typder, exprder) = derive_expr_in_context e context in
				if typder = func.rtyp then SReturn (typder, exprder)
				else 
					if typder = Void then 
						SReturn(typder, exprder)
					else
						raise (Failure ("illegal return: return gives " ^ string_of_typ typder ^ " expected "
											(*^ string_of_typ func.rtyp ^ " in " ^ string_of_expr exprder*)
											))
			| Block sl -> 
				let rec check_stmt_list = function
				 [Return _ as s ] -> [check_stmt s]
				 | Return _ :: _ -> raise (Failure "Illegal statements after return")
				 | Block sl :: ss -> check_stmt_list (sl @ ss) (* not sure of the point of this *)
				 | s :: ss       -> check_stmt s :: check_stmt_list ss
				 | [] 			 -> []
				in SBlock(check_stmt_list sl)
			
	in  {
		srtyp 		= func.rtyp;
		sfname		= func.fname;
		sformals 	= sformals;
		slocals		= slocals;
		sbody		= match check_stmt (Block func.body) with
			SBlock(sl) -> sl
			| _ -> raise (Failure ("internal error: could not convert block"))
	} 
in 

(*Convert global binds to Sbinds*)
let store_binds binds = let get_one = function 
			| AssignBind(t,n,e) -> SAssignBind(t,n,(derive_expr_in_context e binds))
			| NoAssignBind(t,n) -> SNoAssignBind(t,n)
			in
			List.map get_one binds
in
let sglobals = store_binds globals in

(sglobals, (List.map check_function functions))
