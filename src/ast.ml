type bop = Add | Sub | Equal | Neq | Less | And | Or

type typ = Int | Bool

type expr =
  | Literal of int
  | BoolLit of bool
  | Id of string
  | Binop of expr * bop * expr
  | Assign of string * expr

type stmt =
  | Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt

type bind = typ * string

type program = {
  locals: bind list;
  body: stmt list;
}
