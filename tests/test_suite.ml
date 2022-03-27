open Src
open OUnit2


let one_test current_test =
  let lexbuf = Lexing.from_string current_test in
  let program = Commaparse.program_rule Scanner.tokenize lexbuf in
  (*print_endline (Ast.string_of_program program)*)
  Ast.string_of_program program

let run_pass_test name input expected_output =
  name >:: (fun _ -> assert_equal (one_test input) expected_output)

let raises input =
  try one_test input
  with Failure err -> err

let run_fail_test name input expected_failure =
  name >:: (fun _ -> assert_raises expected_failure (fun () -> raises input))

let parse_tests = "Test suite for parser" >::: [
                  run_pass_test "int dclr" "int a;" "int a;\n";
                  run_pass_test "int decl w/ assign" "int a = 7;" "int a = 7;\n";
                  run_pass_test "int decl w/ assign var" "int a = 1; int b = a;" "int a = 1;\nint b = a;\n";
                  run_fail_test "int decl w/ assign illegal" "int a = &" Stdlib.Parsing.Parse_error;
                  run_fail_test "int decl w/ illegal var name" "int 4;" Stdlib.Parsing.Parse_error;
                  run_fail_test "int decl w/ illegal var name" "int double;" Stdlib.Parsing.Parse_error;
                  run_pass_test "bool dclr" "bool a;" "bool a;\n";
                  run_pass_test "bool decl w/ assign" "bool a = true;" "bool a = true;\n";
                  run_pass_test "bool decl w/ assign var" "bool a = true; bool b = true;" "bool a = true;\nbool b = true;\n";
                  run_fail_test "bool decl w/ assign illegal" "bool a = )" Stdlib.Parsing.Parse_error;
                  run_fail_test "bool decl w/ illegal var name" "bool double;" Stdlib.Parsing.Parse_error;
                  
				  run_pass_test "if expr w/ empty stmt" "if (true) { }" "if (true)\n{\n}\nelse\n{\n}\n";
				  run_fail_test "if expr w/ empty stmt" "if (true;) { }" Stdlib.Parsing.Parse_error;
				  ]
(*
                  run_pass_test "double decl" "double b;" "double b\n"; 
                  run_pass_test "double decl w/ assign" "double b = 3.1;" "double b = 3.1;\n";
                  run_fail_test "double decl w/ illegal assign" "double b = 3;" Stdlib.Parsing.Parse_error;
                  run_fail_test "double decl w/ illegal var name" "double %;" Stdlib.Parsing.Parse_error;
                  run_fail_test "double decl w/ illegal var name" "double int" Stdlib.Parsing.Parse_error
                  run_pass_test "char decl" "char b;" "char b\n"; 
                  run_pass_test "char decl w/ assign" "char b = 'a';" "'a'\n";
                  run_fail_test "char decl w/ illegal assign" "char b = 3;" Stdlib.Parsing.Parse_error;
                  run_fail_test "char decl w/ illegal var name" "char %;" Stdlib.Parsing.Parse_error;
                  run_fail_test "char decl w/ illegal var name" "char int" Stdlib.Parsing.Parse_error
                  run_pass_test "array decl" "double[] b;" "double[] b;\n"; 
                  run_pass_test "array decl w/ assign" "int[] b = [3, -4];" "int[] b = [3, -4];\n";
                  run_fail_test "array decl w/ illegal assign" "double[] b = 3;" Stdlib.Parsing.Parse_error;
                  run_fail_test "array decl w/ illegal assign" "bool[] %;" Stdlib.Parsing.Parse_error;
                  run_fail_test "array decl w/ illegal var name" "int[] int" Stdlib.Parsing.Parse_error
*)                  
let _ = run_test_tt_main parse_tests
