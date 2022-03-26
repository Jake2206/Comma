open Src
open OUnit2


let one_test current_test =
  let lexbuf = Lexing.from_string current_test in
  let program = Commaparse.program_rule Scanner.tokenize lexbuf in
  (*print_endline (Ast.string_of_program program)*)
  Ast.string_of_program program

let get_str s =
  s

let run_pass_test name input expected_output =
  name >:: (fun _ -> assert_equal expected_output (one_test input) ~printer: get_str)

let run_fail_test name input expected_failure =
  name >:: (fun _ -> assert_raises expected_failure (fun () -> one_test input))


let parse_tests = "Test suite for parser" >::: [
                  run_pass_test "int decl w/ assign" "int a = 7;" "int a = 7;\n";
                  run_fail_test "int decl w/ assign illegal" "int a = &" (Failure("illegal character &"));
                  run_fail_test "int decl w/ illegal var name1" "int 4;" Stdlib.Parsing.Parse_error;
                  run_fail_test "int decl w/ illegal var name2" "int double;" Stdlib.Parsing.Parse_error;
                  run_pass_test "bool dclr w/ assign" "bool a = true;" "bool a = true;\n"; 
                  run_fail_test "bool decl w/ assign illegal" "bool a = )" Stdlib.Parsing.Parse_error;
                  run_fail_test "bool decl w/ illegal var name" "bool double;" Stdlib.Parsing.Parse_error;
                  run_pass_test "double decl w/ assign" "double b = 3.2;" "double b = 3.2;\n"; 
                  run_fail_test "double decl w/ illegal var name1" "double %;" (Failure("illegal character %"));
                  run_fail_test "double decl w/ illegal var name2" "double int" Stdlib.Parsing.Parse_error;
                  run_pass_test "char decl" "char b = 'a';" "char b = 'a';\n"; 
                  run_fail_test "char decl w/ illegal var name1" "char %;" (Failure("illegal character %"));
                  run_fail_test "char decl w/ illegal var name2" "char int" Stdlib.Parsing.Parse_error;
                  run_pass_test "list decl w/ assign" "int[] b = [3, 4];" "int b = [34];\n";
                  run_pass_test "list decl w/ assign" "int[] b = [3, 't'];" "int b = [3't'];\n";
                  run_fail_test "list decl w/ illegal assign" "bool[] %;" (Failure("illegal character %"));
                  run_fail_test "list decl w/ illegal var name" "int[] int" Stdlib.Parsing.Parse_error
                  ]
let _ = run_test_tt_main parse_tests
