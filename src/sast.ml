open Ast

type sexpr = typ * sx

type sx =
  | SIntLit of int
  | SNul
  | SBoolLit of bool
  | SCharLit of char
  | SDoubLit of float
  | SListLit of sexpr list
  | SId of string
  | SBinop of sexpr * bop * sexpr
  | SAssign of string * sexpr

type sstmt =
  | SBlock of sstmt list
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SWhile of sexpr * sstmt
  | SFor of sexpr * sexpr * sexpr * sstmt
  | SReturn of sexpr

type sbind = typ * string * sexpr

type sprogram = {
  slocals: bind list;
  sbody: sstmt list;
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

let string_of_sarray a =
  let buf = Buffer.create 2000 in
  List.iter (Buffer.add_string buf) a;
  Buffer.contents buf

let rec string_of_sexpr = function
    SIntLit(l) -> string_of_int l
  | NulLit     -> "nul" 
  | SCharLit(c) -> "'" ^ String.make 1 c ^ "'"
  | SDoubLit(d) -> string_of_float d
  | SBoolLit(true) -> "true"
  | SBoolLit(false) -> "false"
  | SListLit(a) -> "[" ^ string_of_sarray (List.map string_of_sexpr a) ^ "]"
  | SId(s) -> s
  | SBinop(e1, o, e2) ->
    string_of_sexpr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_sexpr e2
  | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e

let rec string_of_sstmt = function
    SBlock(sstmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt sstmts) ^ "}\n"
  | SExpr(sexpr) -> string_of_sexpr sexpr ^ ";\n";
  | SReturn(sexpr) -> "return " ^ string_of_sexpr sexpr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                      string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Double -> "double"
  | Char -> "char"
  | List -> "[]"
  | Nul  -> "nul" 

let string_of_svdecl (t, id, lit) = string_of_typ t ^ " " ^ id ^ " = " ^ string_of_sexpr lit ^ ";\n"

let string_of_sprogram fdecl =
  String.concat "" (List.map string_of_svdecl fdecl.slocals) ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) 
