type bop = Add | Sub | Equal | Neq | Less | Great | LessEqual | GreatEqual | And | Or 

type typ = Int | Bool | Double | Char | List (*| Array | Matrix | Nul*)

type expr =
  | IntLit of int
  | BoolLit of bool
  | CharLit of char
  | DoubLit of float
  | ListLit of expr list
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  (* | For of expr * expr * expr * stmt *)

type bind = typ * string * expr

type program = {
  locals: bind list;
  body: stmt list;
}

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
  | CharLit(c) -> "'" ^ String.make 1 c ^ "'"
  | DoubLit(d) -> string_of_float d
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | ListLit(a) -> "[" ^ string_of_array (List.map string_of_expr a) ^ "]"
  | Id(s) -> s
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Double -> "double"
  | Char -> "char"
  | List -> "[]"

let string_of_vdecl (t, id, lit) = string_of_typ t ^ " " ^ id ^ " = " ^ string_of_expr lit ^ ";\n"

let string_of_program fdecl =
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) 
