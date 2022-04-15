open Src

let run_test current_test =
  let lexbuf = Lexing.from_string current_test in
  let program = Commaparse.program_rule Scanner.tokenize lexbuf in
  let sprogram = Semant.check program in
  print_endline (Sast.string_of_sprogram sprogram)


let%expect_test "int decl w/ assign" = run_test "def int main() { int b = 3; }"; 
  [%expect {|
    def int main()
    {
    int b = 3;
    }
    |}]

let%expect_test "multiply expr in main" = run_test "def int main() { 2*5; }"; 
  [%expect {|
    def int main()
    {
    (int : (int : 2) * (int : 5));
    } |}];;

let%expect_test "divide expr in func" = run_test "def int main() { 2/5; }"; 
  [%expect {|
    def int main()
    {
    (int : (int : 2) / (int : 5));
    } |}];;

let%expect_test "if else stmt" = run_test "def int main() { int a = 2; if(a==2){a = 2;}else{a = 3;} }"; 
  [%expect {|
    def int main()
    {
    int a = 2;
    if ((bool : (int : a) == (int : 2)))
    {
    (int : a = (int : 2));
    }
    else
    {
    (int : a = (int : 3));
    }
    } |}];;

let%expect_test "bool decl w/ assign" = run_test "def int main() { bool a = true; }"; 
  [%expect {|
    def int main()
    {
    bool a = true;
    } |}];;

let%expect_test "double decl w/ assign" = run_test "def int main() { double b = 3.2; }"; 
  [%expect {|
    def int main()
    {
    double b = 3.2;
    } |}];;

let%expect_test "char decl w/ assign" = run_test "def int main() { char b = 'a'; }"; 
  [%expect {|
    def int main()
    {
    char b = 'a';
    } |}];;

let%expect_test "func declr" = run_test "def int main() {  } def int test_f ( int a, int b ) { return a+b; }"; 
  [%expect {|
    def int main()
    {
    }
    def int test_f(int a, int b)
    {
    return (int : (int : a) + (int : b));
    } |}];;

let%expect_test "func declr w/ vars" = run_test "def int main() { } def int test_f ( int a, int b ) { int c = 9; return a+b+c; }"; 
  [%expect {|
    def int main()
    {
    }
    def int test_f(int a, int b)
    {
    int c = 9;
    return (int : (int : (int : a) + (int : b)) + (int : c));
    } |}];;

let%expect_test "func declr w/ if else" = run_test "def int main() { } def int test_f ( int a, int b ) { int c = 9; if(a==b){return a;}else{return c;}}"; 
  [%expect {|
    def int main()
    {
    }
    def int test_f(int a, int b)
    {
    int c = 9;
    if ((bool : (int : a) == (int : b)))
    {
    return (int : a);
    }
    else
    {
    return (int : c);
    }
    } |}];;

let%expect_test "func decl w/ while stmt" = run_test "def int main() { } def int test_f ( int a, int b ) { int c = 9; while(a>b){a=a-1;}return a;}"; 
  [%expect {|
    def int main()
    {
    }
    def int test_f(int a, int b)
    {
    int c = 9;
    while ((bool : (int : a) > (int : b))) {
    (int : a = (int : (int : a) - (int : 1)));
    }
    return (int : a);
    } |}];;

let%expect_test _ = run_test "def int main() { } int g = 10; def int test_f ( int a, int b ) { int c = 9; if(a==b){return a;}else{return c;}}"; 
  [%expect {|
    int g = 10;

    def int main()
    {
    }
    def int test_f(int a, int b)
    {
    int c = 9;
    if ((bool : (int : a) == (int : b)))
    {
    return (int : a);
    }
    else
    {
    return (int : c);
    }
    } |}];;
(*

let%expect_test _ = run_test "def int main() { if (true) { } eif (false) {} }"; 
  [%expect {|
    def int main()
    {
    bool a = true;
    } |}];;

let%expect_test _ = run_test "def int main() { if (true) { } }"; 
  [%expect {|
    def int main()
    {
    bool a = true;
    } |}];;

let%expect_test _ = run_test "def int main() { int[] b = int [3, 4]; }"; 
  [%expect {|
    def int main()
    {
    char b = 'a';
    } |}];;

let%expect_test _ = run_test "def int main() { matrix [[3.1, 4.1],[3.1, 4.1]]; }"; 
  [%expect {|
    def int main()
    {
    char b = 'a';
    } |}];;

*)


(* Failure Tests begin here 
  NOTE: our custom exceptions are caught as generic failures but ocaml 
  exceptions need to be caught with a specific error *)

let%expect_test "Bad character" = 
  try run_test "def int main() { int b = &; }" with 
    Failure e -> print_endline e;
  [%expect {|
    illegal character &
    |}]

let%expect_test "Mismatched type int decl" = 
  try run_test "def int main() { int b = 'a'; }" with 
    Failure e -> print_endline e;
  [%expect {|
    Illegal bind: mismatched types: expected 'int' got 'char' instead in expr: 'a'
    |}]

let%expect_test "int decl w/ illegal var name" = 
  try run_test "def int main() { int double; }" with 
    Parsing.Parse_error -> print_endline "Parsing.Parse_error";
  [%expect {|
    Parsing.Parse_error
    |}]

let%expect_test "if expr w/ illegal semi" = 
  try run_test "def int main() { if (true;) { } }" with 
    Parsing.Parse_error -> print_endline "Parsing.Parse_error";
  [%expect {|
    Parsing.Parse_error
    |}]


let%expect_test "func decl w/ no types in parameters" = 
  try run_test "def int main() { } def int test_f ( a, b ) { return a+b; }" with 
    Parsing.Parse_error -> print_endline "Parsing.Parse_error";
  [%expect {|
    Parsing.Parse_error
    |}]

let%expect_test "func declr w/ no return type" = 
  try run_test "def int main() { } def test_f ( a, b ) { return a+b; }" with 
    Parsing.Parse_error -> print_endline "Parsing.Parse_error";
  [%expect {|
    Parsing.Parse_error
    |}]



