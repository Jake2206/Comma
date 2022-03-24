(* open OUnit2 *)
open Src

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let program = Commaparse.program_rule Scanner.tokenize lexbuf in
  print_endline (Ast.string_of_program program)


