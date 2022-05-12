type bop =  Multiply | Divide | Add | Sub | Equal | Neq | Less | Great | LessEqual | GreatEqual | And | Or | Mod
type uop = Neg | Not

type typ = Int | Bool | Double | Char | Void | Array of typ | Matrix 

type expr =
  | NulLit 
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | DoubLit of string
  | ArrayLit of typ * expr list
  | MatrixLit of (expr list) list
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr
  | Call of string * expr list
  | Lambda of typ * string * expr list * expr

type bind = 
  AssignBind of typ * string * expr
  | NoAssignBind of typ * string 

type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * stmt
  | Return of expr

type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Multiply -> "*"
  | Divide -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Great -> ">"
  | LessEqual -> "<="
  | GreatEqual -> ">="
  | And -> "&&"
  | Or -> "||"
  | Mod -> "%"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Double -> "double"
  | Char -> "char"
  | Array(_) -> "array"
  | Matrix -> "matrix"
  | Void -> "void"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | NulLit     -> "nul" 
  | CharLit(c) -> "'" ^ String.make 1 c ^ "'"
  | DoubLit(d) -> d
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ArrayLit(t, a) ->  "[" ^ String.concat ", " (List.map string_of_expr a) ^ "] " ^ string_of_typ t
  | MatrixLit(m) -> "|[[" ^ String.concat "],[" (List.map (fun a -> String.concat ", " (List.map string_of_expr a)) m) ^ "]]| "
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) -> f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Lambda(typ, id, e, arg) -> "@ " ^ string_of_typ typ ^ id ^ "{ " ^ String.concat "\n" (List.map string_of_expr e) ^ " }" ^ string_of_expr arg

let string_of_vdecl bind = 
  match bind with 
  AssignBind(t, i, e) -> string_of_typ t ^ " " ^ i ^ " = " ^ string_of_expr e ^ ";\n"
  | NoAssignBind(t, i) -> string_of_typ t ^ " " ^ i ^ ";\n"
  
let string_of_args bind = 
  match bind with
  AssignBind(t, i, e) -> string_of_typ t ^ " " ^ i ^ " = " ^ string_of_expr e
  | NoAssignBind(t, i) -> string_of_typ t ^ " " ^ i

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For (e2, e3, s) -> "for (" ^ ", " ^ string_of_expr e2 ^ ", " ^ string_of_expr e3 ^ string_of_stmt s

let string_of_fdecl fdecl =
  "def " ^ string_of_typ fdecl.rtyp ^ " " ^ fdecl.fname ^ "(" ^ String.concat ", " 
  (List.map (fun x -> string_of_args x) fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^
  String.concat "" (List.map string_of_fdecl funcs)
