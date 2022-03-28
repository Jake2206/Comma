
open Ast
open Sast

module StringMap = Map.Make(String)

type func_symbol = func_decl StringMap.t

let check (globals, functions) = 
	let check_binds (kind  : string) (binds : bind list) =
		(* Check variables bind to a real type (not null)*)
		List.iter (function
			(Nul, b) -> raise (Failure ("illegal bind: cannot be of type nul"))
			| _ -> ()) binds;
		
		(* Check no two variables have same name within same scope. *)
		let rec dups = function 
			[] -> ()
			| ((_, n1) :: (_, n2) :: _) when n1 = n2 -> raise (Failure ("duplicate declaration"))
			| _ :: t -> dups t
		in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds) 
	in 
	
	(* Check global variables *)
	check_binds "globals" globals;
	
	(* Check built-in functions *)
	
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
	let function_decls = List.fold_left add_func built_in_decls functions
	in 

	(* Find function in table *)
	let find_func s =
		try StringMap.find s function_decls
		with Not_found -> raise (Failure ("no such function declared: " ^ s))
	in
	
	let check_function func =
		check_binds "formal" func.formals;
		check_binds "local" func.locals;
	
	(* Check assignment (types match) *)
	let check_assign lvaluetype rvaluetype err =
		if lvaluetype = rvaluetype then lvaluetype else raise (Failure err) 
	in
	
	(* Variable table: keep track of type global, formal, local
		-> formal variables are arguments passed to a function *)
	let symbols = List.fold_left 
					(fun m (t, n) -> StringMap.add n t m) StringMap.empty
											( globals @ func.formals @ func.locals )
	in 
	
	let type_of_identifier s =
		try StringMap.find s symbols
		with Not_found -> raise (Failure ("undeclared symbol: " ^ s))
	in 						

	let rec expr = function
		NulLit -> (Nul, SNulLit)
		| IntLit l -> (Int, SIntLit l)
		| BoolLit l -> (Bool, SBoolLit l)
		| CharLit l -> (Char, SCharLit l)
		| DoubLit l -> (Double, SDoubLit l)
		| ListLit l -> (List, SListLit l)
		| Id l      -> (type_of_identifier l, SId s)
		| Binop(e1, op, e2) as bin -> 
				let (lt, e1derived) = expr e1
				and (rt, e2derived) = expr e2 in
				let same = lt = rt in
				let ty = match op with
					Add | Sub | Mult | Div when same && lt = Int  -> Int
					| Add | Sub | Mult | Div when same && lt = Float -> Float
					| Equal | Neq            when same 				-> Bool
					| Less | Great | LessEqual | GreatEqual 
											 when same && (lt = Int || lt = Float) -> Bool
				    | And | Or when same && lt = Bool 				-> Bool
					| _ -> raise (Failure ("illegal binary operation"))
				in (ty, SBinop((lt, e1derived), op, (rt, e2derived)))
		| Assign(var, e) as ex -> 
				let lt = type_of_identifier var
				and (rt, ederived) = expr e in
				let err = "Illegal assignment: " ^ 
						string_of_typ lt ^ " = " ^ string_of_type rt ^ " in " ^ 
						string_of_expr ex
				in (check_assign lt rt err, SAssign(var, (rt, ederived)))
		(* Need to add a case for function calls *)
	in
	
	(* Check expression returns a boolean *)
	let chec_bool_expr e = 
		let (typder, exprder) = expr e
		and err = "Expected Boolean expression"
		in if typder != Bool then raise (Failure err) else (typder, exprder)
	in

	(* Check statement *)
	let rec check_stmt = function
		Expr e -> SExpr (expr e)
		| If(p, b1, b2) -> SIf(check_bool_expr p, check_stmt b1, check_stmt b2)
		| For(e1, e2, e3, st) -> SFor(expr e1, check_bool_expr e2, expr e3, check_stmt st)
		| While (p, s) -> SWhile(check_bool_expr p, check_stmt s)
		| Return e -> let (typder, exprder) = expr e in
			if typder = func.typ then SReturn (typder, exprder)
			else raise (Failure ("return gives " ^ string_of_typ typder ^ " expected " ^
								string_of_typ func.typ ^ " in " ^ string_of_expr exprder))
		| Block sl -> 
			let rec check_stmt_list = function
			 [Return _ as s ] -> [check_stmt s]
			 | Return _ :: _ -> raise (Failure "Illegal statements after return")
			 | Block sl :: ss -> check_stmt_list (sl @ ss) (* not sure of the point of this *)
			 | s :: ss       -> check_stmt s :: check_stmt_list ss
			 | [] 			 -> []
			in SBlock(check_stmt_list sl)
		
	in {
		styp 		= func.typ;
		sfname		= func.fname;
		sformals 	= func.formals;
		slocals		= func.locals;
		sbody		= match check_stmt (Block func.body) with
			SBlock(sl) -> sl
			| _ -> raise (Failure ("internal error: could not convert block"))	
	}
	
in (globals, List.map check_function functions)