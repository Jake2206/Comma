open Src
open OUnit2



let one_test current_test =
  let lexbuf = Lexing.from_string current_test in
  let program = Commaparse.program_rule Scanner.tokenize lexbuf in
  (*print_endline (Ast.string_of_program program);*)
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

                  run_pass_test "stmt outside func" "a = 7;" "a = 7;\n";
                  run_pass_test "multiply stmt outside func" "2*5;" "2 * 5;\n";
                  run_pass_test "multiply stmt outside func" "2/5;" "2 / 5;\n";
                  run_pass_test "if stmt" "if(a==2){return 2;}else{return 5;}" "if (a == 2)\n{\nreturn 2;\n}\nelse\n{\nreturn 5;\n}\n";

                  run_pass_test "bool dclr w/ assign" "bool a = true;" "bool a = true;\n"; 
                  run_fail_test "bool decl w/ assign illegal" "bool a = )" Stdlib.Parsing.Parse_error;
                  run_fail_test "bool decl w/ illegal var name" "bool double;" Stdlib.Parsing.Parse_error;

                  run_pass_test "if expr w/ empty stmt" "if (true) { }" "if (true)\n{\n}\nelse\n{\n}\n";
                  run_fail_test "if expr w/ empty stmt" "if (true;) { }" Stdlib.Parsing.Parse_error;
                  run_pass_test "if expr w/ eif" "if (true) { } eif (false) {}" "if (true)\n{\n}\nelse\nif (false)\n{\n}\nelse\n{\n}\n";

                  run_pass_test "double decl w/ assign" "double b = 3.2;" "double b = 3.2;\n"; 
                  run_fail_test "double decl w/ illegal var name1" "double %;" (Failure("illegal character %"));
                  run_fail_test "double decl w/ illegal var name2" "double int" Stdlib.Parsing.Parse_error;

                  run_pass_test "char decl" "char b = 'a';" "char b = 'a';\n"; 
                  run_fail_test "char decl w/ illegal var name1" "char %;" (Failure("illegal character %"));
                  run_fail_test "char decl w/ illegal var name2" "char int" Stdlib.Parsing.Parse_error;

                  run_pass_test "list decl w/ assign" "int[] b = [3, 4];" "int b = [34];\n";
                  run_pass_test "list decl w/ assign" "int[] b = [3, 't'];" "int b = [3't'];\n";
                  run_fail_test "list decl w/ illegal assign" "bool[] %;" (Failure("illegal character %"));
                  run_fail_test "list decl w/ illegal var name" "int[] int" Stdlib.Parsing.Parse_error;
		  run_pass_test "char list decl" "char[] s = ['t', 'e', 's', 't'];" "char s = [test];\n";
		  run_fail_test "print test char" "print('t');" "t\n";
		  run_pass_test "print test int" "print(2);" "2\n";
                  run_pass_test "if expr w/ empty stmt" "if (true) { }" "if (true)\n{\n}\nelse\n{\n}\n";
				  run_fail_test "if expr w/ empty stmt" "if (true;) { }" Stdlib.Parsing.Parse_error;
                  run_pass_test "hello world" "char[] helloworld = ['h', 'e', 'l', 'l', 'o', ' ', 'w', 'o', 'r', 'l', 'd', '!']; char[] newlist = [print(ele) for ele in helloworld];" "hello world!\n";`

                  run_pass_test "func declr" "def int test_f ( int a, int b ) { return a+b; }" "def int test_f(int a, int b)\n{\nreturn a + b;\n}\n";
                  run_fail_test "func declr w/ no types in parameters" "def int test_f ( a, b ) { return a+b; }" Stdlib.Parsing.Parse_error;
                  run_fail_test "func declr w/ no return type" "def test_f ( a, b ) { return a+b; }" Stdlib.Parsing.Parse_error;
                  run_pass_test "func declr w/ vars" "def int test_f ( int a, int b ) { int c = 9; return a+b+c; }" "def int test_f(int a, int b)\n{\nint c = 9;\nreturn a + b + c;\n}\n";
                  run_pass_test "func declr w/ if stmt" "def int test_f ( int a, int b ) { int c = 9; if(a==b){return a;}else{return c;}}" "def int test_f(int a, int b)\n{\nint c = 9;\nif (a == b)\n{\nreturn a;\n}\nelse\n{\nreturn c;\n}\n}\n";
                  run_pass_test "func declr w/ while stmt" "def int test_f ( int a, int b ) { int c = 9; while(a>b){a=a-1;}return a;}" "def int test_f(int a, int b)\n{\nint c = 9;\nwhile (a > b) {\na = a - 1;\n}\nreturn a;\n}\n";
                  run_pass_test "local variable declr and func declr w/ if stmt" "int g = 10; def int test_f ( int a, int b ) { int c = 9; if(a==b){return a;}else{return c;}}" "int g = 10;\ndef int test_f(int a, int b)\n{\nint c = 9;\nif (a == b)\n{\nreturn a;\n}\nelse\n{\nreturn c;\n}\n}\n";
                  
let _ = run_test_tt_main parse_tests
