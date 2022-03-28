type bop = Add | Sub | Equal | Neq | Less | Great | LessEqual | GreatEqual | And | Or 
type uop = Neg | Not

type typ = Int | Bool | Double | Char | List | Nul  (*| Array | Matrix *)

type expr =
  | NulLit 
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | DoubLit of float
  | ListLit of expr list
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr
  | Call of string * expr list


type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  | For of expr * expr * expr * stmt
  | Return of expr

type bind = typ * string * expr
type bind_no_assign = typ * string

type func_def = {
  rtyp: typ;
  fname: string;
  formals: bind_no_assign list;
  locals: bind list;
  body: stmt list;
}

type program = bind list * stmt list * func_def list



(* Pretty-printing functions *)
let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Great -> ">"
  | LessEqual -> "<="
  | GreatEqual -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_array a =
  let buf = Buffer.create 2000 in
  List.iter (Buffer.add_string buf) a;
  Buffer.contents buf

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | NulLit     -> "nul" 
  | CharLit(c) -> "'" ^ String.make 1 c ^ "'"
  | DoubLit(d) -> string_of_float d
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ListLit(a) -> "[" ^ string_of_array (List.map string_of_expr a) ^ "]"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Call(f, el) ->
    f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | For (e1, e2, e3, s) -> "for (" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ", " ^ string_of_expr e3 ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Double -> "double"
  | Char -> "char"
  | List -> "[]"
  | Nul -> "nul" 

let string_of_vdecl (t, id, lit) = string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr lit ^ ";\n"


let string_of_fdecl fdecl =
  "def " ^ string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map (fun x -> (string_of_typ (fst x)) ^ " " ^ (snd x))fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, stmts, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^
  String.concat "" (List.map string_of_stmt stmts) ^
  String.concat "" (List.map string_of_fdecl funcs)
