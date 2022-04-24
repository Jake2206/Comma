
open Ast

type sexpr = typ * sx

and sx =
   SNulLit
  | SIntLit of int
  | SBoolLit of bool
  | SCharLit of char
  | SDoubLit of float
  | SArrayLit of typ * sexpr list
  | SMatrixLit of typ * (sexpr list) list
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr
  | SCall of string * sexpr list
  | SLambda of typ * string * sexpr list

type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SReturn of sexpr

type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: bind list;
  slocals: bind list;
  sbody: sstmt list;
}

type sprogram = {
  slocals: bind list;
  sfunctions: sfunc_def list;
}

(* Pretty-printing functions *)

let string_of_slist a =
  let buf = Buffer.create 2000 in
  List.iter (Buffer.add_string buf) a;
  Buffer.contents buf

let rec string_of_sexpr (t, e) = 
	"(" ^ string_of_typ t ^ " : " ^
	(match e with
		SIntLit(l) -> string_of_int l
	  | SNulLit     -> "nul" 
	  | SCharLit(c) -> "'" ^ String.make 1 c ^ "'"
	  | SDoubLit(d) -> string_of_float d
	  | SBoolLit(true) -> "true"
	  | SBoolLit(false) -> "false"
    | SMatrixLit(t, m) -> "|[" ^ string_of_slist (List.map (fun a -> string_of_slist (List.map string_of_sexpr a)) m) ^ "]| " ^ string_of_typ t
	  | SArrayLit(t, a) ->  "[" ^ (String.concat ", " (List.map string_of_sexpr a)) ^ "] " ^ string_of_typ t
	  | SId(s) -> s
	  | SBinop(e1, o, e2) ->
		string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
	  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
	  | SCall(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    | SLambda(typ, id, e) -> "@ " ^ string_of_typ typ ^ id ^ "{ " ^ String.concat "\n" (List.map string_of_sexpr e) ^ " }"
	) ^ ")"
	
let rec string_of_sstmt = function
    SBlock(sstmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt sstmts) ^ "}\n"
  | SExpr(sexpr) -> string_of_sexpr sexpr ^ ";\n";
  | SReturn(sexpr) -> "return " ^ string_of_sexpr sexpr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SFor (e1, e2, e3, s) -> "for (" ^ string_of_sexpr e1 ^ ", " ^ string_of_sexpr e2 ^ ", " ^ string_of_sexpr e3 ^ string_of_sstmt s

let string_of_sfdecl fdecl =
  "def " ^ string_of_typ fdecl.srtyp ^ " " ^ fdecl.sfname ^ "(" ^ String.concat ", " 
  (List.map (fun x -> string_of_args x) fdecl.sformals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (binds, fdecls) =
  String.concat "" (List.map string_of_vdecl binds) ^ "\n" ^
  String.concat "" (List.map string_of_sfdecl fdecls)
  
  

