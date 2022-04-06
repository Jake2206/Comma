open Src
open OUnit2


let one_test current_test =
  let lexbuf = Lexing.from_string current_test in
  let program = Commaparse.program_rule Scanner.tokenize lexbuf in
  (*print_endline (Ast.string_of_program program);*)
  let sprogram = Semant.check program in
  Sast.string_of_sprogram sprogram

let get_str s =
  s

let run_pass_test name input expected_output =
  name >:: (fun _ -> assert_equal expected_output (one_test input) ~printer: get_str)

let run_fail_test name input expected_failure =
  name >:: (fun _ -> assert_raises expected_failure (fun () -> one_test input))


let parse_tests = "Test suite for parser" >::: [
                  run_pass_test "int decl w/ assign" "int a = 7;" "int a = 7;\n\n\n";
                  run_fail_test "int decl w/ assign illegal" "int a = &" (Failure("illegal character &"));
                  (*run_fail_test "int decl w/ sem fail assign" "int a = 3.0;" SHOULD RETURN A SEMANTIC ERROR*)
                  run_fail_test "int decl w/ illegal var name1" "int 4;" Stdlib.Parsing.Parse_error;
                  run_fail_test "int decl w/ illegal var name2" "int double;" Stdlib.Parsing.Parse_error;

                  run_pass_test "stmt outside func" "int a = 7;" "int a = 7;\n\n\n";
                  run_pass_test "multiply stmt outside func" "2*5;" "\n(int : (int : 2) * (int : 5));\n\n";
                  run_pass_test "multiply stmt outside func" "2/5;" "\n(int : (int : 2) / (int : 5));\n\n";
                  run_pass_test "if stmt" "int a = 2; if(a==2){a = 2;}else{a = 3;}" "int a = 2;\n\nif ((bool : (int : a) == (int : 2)))\n{\n(int : a = (int : 2));\n}\nelse\n{\n(int : a = (int : 3));\n}\n\n";
                  run_fail_test "if stmt" "if(a==2){return 2;}else{return 5;}" (Failure("illegal return: outside function declaration"));

                  run_pass_test "bool dclr w/ assign" "bool a = true;" "bool a = true;\n\n\n"; 
                  run_fail_test "bool decl w/ assign illegal" "bool a = )" Stdlib.Parsing.Parse_error;
                  run_fail_test "bool decl w/ illegal var name" "bool double;" Stdlib.Parsing.Parse_error;

                  run_pass_test "if expr w/ empty stmt" "if (true) { }" "\nif ((bool : true))\n{\n}\nelse\n{\n}\n\n";
                  run_fail_test "if expr w/ empty stmt" "if (true;) { }" Stdlib.Parsing.Parse_error;
                  run_pass_test "if expr w/ eif" "if (true) { } eif (false) {}" "\nif ((bool : true))\n{\n}\nelse\nif ((bool : false))\n{\n}\nelse\n{\n}\n\n";

                  run_pass_test "double decl w/ assign" "double b = 3.2;" "double b = 3.2;\n\n\n"; 
                  run_fail_test "double decl w/ illegal var name1" "double %;" (Failure("illegal character %"));
                  run_fail_test "double decl w/ illegal var name2" "double int" Stdlib.Parsing.Parse_error;

                  run_pass_test "char decl" "char b = 'a';" "char b = 'a';\n\n\n"; 
                  run_fail_test "char decl w/ illegal var name1" "char %;" (Failure("illegal character %"));
                  run_fail_test "char decl w/ illegal var name2" "char int" Stdlib.Parsing.Parse_error;

                  run_pass_test "array decl w/ assign" "int[] b = int [3, 4];" "int b = [34];\n\n\n";
                  (*run_fail_test "array decl w/ mixed types" "int[] b = int [3, 't'];" "int b = [3't'];\n\n\n";*)
                  run_fail_test "array decl w/ illegal assign" "bool[] %;" (Failure("illegal character %"));
                  run_fail_test "array decl w/ illegal var name" "int[] int" Stdlib.Parsing.Parse_error;
                  
                  run_pass_test "matrix decl w/ assign" "matrix[] b = int [[3, 4],[3, 4]];" "[][] b = [[34][34]];\n\n\n";
                  run_fail_test "matrix decl w/ missing comma" "matrix[] b = int [[3, 4],[3, 4][1]];" Stdlib.Parsing.Parse_error;
                  (*run_fail_test "matrix decl w/ incorrect dims" "matrix[] b = int [[3, 4],[3, 4],[1]];" Stdlib.Parsing.Parse_error;*)
                  (*run_fail_test "matrix decl w/ non-double types" "matrix[] b = int [[3, 't'],[3.5, 1, 'a']];" "[][] b = [[3't'][3.51'a']];\n\n\n"; *)

                  run_fail_test "func declr" "def int test_f ( int a ) { return a = 3.0; }" (Failure("Illegal assignment: int = double in a = 3."));
                  run_pass_test "func declr" "def int test_f ( int a, int b ) { return a+b; }" "\n\ndef int test_f(int a, int b)\n{\nreturn (int : (int : a) + (int : b));\n}\n";
                  run_fail_test "func declr w/ no types in parameters" "def int test_f ( a, b ) { return a+b; }" Stdlib.Parsing.Parse_error;
                  run_fail_test "func declr w/ no return type" "def test_f ( a, b ) { return a+b; }" Stdlib.Parsing.Parse_error;
                  run_pass_test "func declr w/ vars" "def int test_f ( int a, int b ) { int c = 9; return a+b+c; }" "\n\ndef int test_f(int a, int b)\n{\nint c = 9;\nreturn (int : (int : (int : a) + (int : b)) + (int : c));\n}\n";
                  run_pass_test "func declr w/ if stmt" "def int test_f ( int a, int b ) { int c = 9; if(a==b){return a;}else{return c;}}" "\n\ndef int test_f(int a, int b)\n{\nint c = 9;\nif ((bool : (int : a) == (int : b)))\n{\nreturn (int : a);\n}\nelse\n{\nreturn (int : c);\n}\n}\n";
                  run_pass_test "func declr w/ while stmt" "def int test_f ( int a, int b ) { int c = 9; while(a>b){a=a-1;}return a;}" "\n\ndef int test_f(int a, int b)\n{\nint c = 9;\nwhile ((bool : (int : a) > (int : b))) {\n(int : a = (int : (int : a) - (int : 1)));\n}\nreturn (int : a);\n}\n";
                  run_pass_test "local variable declr and func declr w/ if stmt" "int g = 10; def int test_f ( int a, int b ) { int c = 9; if(a==b){return a;}else{return c;}}" "int g = 10;\n\n\ndef int test_f(int a, int b)\n{\nint c = 9;\nif ((bool : (int : a) == (int : b)))\n{\nreturn (int : a);\n}\nelse\n{\nreturn (int : c);\n}\n}\n";
                  
]
let _ = run_test_tt_main parse_tests
