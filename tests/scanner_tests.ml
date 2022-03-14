open OUnit2
open Scanner






let _ =
  let lexbuf = Lexing.from_channel stdin in
  let tokens = Scanner.tokenize lexbuf in
  print_endline (string_of_program tokens)


