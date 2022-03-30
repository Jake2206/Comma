(*

open Src
let () = print_int (Helpers.get_third_item_in_tuple (5,6,7) );


*)

open Src

open Sast

let _ =
	let lexbuf = Lexing.from_channel stdin in
	let program = Commaparse.program_rule Scanner.tokenize lexbuf in
	let sprogram = Semant.check program in
	print_endline (string_of_sprogram sprogram)