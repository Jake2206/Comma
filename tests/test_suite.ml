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
let%expect_test "global int decl w/ assign" = run_test "int a = 5; def int main() { int b = 3; }"; 
  [%expect {|
    int a = 5;

    def int main()
    {
    int b = 3;
    }
    |}];;
let%expect_test "global bool decl w/ assign" = run_test "bool a = false; def int main() { int b = 3; }"; 
  [%expect {|
    bool a = false;
    
    def int main()
    {
    int b = 3;
    }
    |}];;
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

let%expect_test "if else stmt" = run_test "def int main() { int a = 2; if(a==2){a = 2;}else{a = 3;} return a;}"; 
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
    return (int : a);
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

let%expect_test "func decl w/ while stmt" = run_test "def int main() { } def int test_f ( int a, int b ) { int c = 9; while(a>b){a=a-1;} return a;}"; 
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

let%expect_test "complex func declr" = run_test "int g = 10; def int main() { } def int test_f ( int a, int b ) { int c = 9; if(a==b){return a;}else{return c;}}"; 
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

let%expect_test "func declr w/ func call" = run_test "def int main() { int a = 1; int b = 1; int c = test_f(a, b); } def int test_f ( int a, int b ) { return a+b; }"; 
  [%expect {|
    def int main()
    {
    int a = 1;
    int b = 1;
    int c = test_f(a, b);
    }
    def int test_f(int a, int b)
    {
    return (int : (int : a) + (int : b));
    } |}];;

let%expect_test "func declr w/ func call as arg" = run_test "def int main() { int c = test_f2(test_f(1,1), test_f(1,1)); } def int test_f ( int a, int b ) { return a+b; } def int test_f2 ( int a, int b ) { return a*b; }"; 
  [%expect {|
    def int main()
    {
    int c = test_f2(test_f(1, 1), test_f(1, 1));
    }
    def int test_f(int a, int b)
    {
    return (int : (int : a) + (int : b));
    }
    def int test_f2(int a, int b)
    {
    return (int : (int : a) * (int : b));
    } |}];;

let%expect_test "if eif" = run_test "def int main() { if (true) { } eif (false) {} }"; 
  [%expect {|
    def int main()
    {
    if ((bool : true))
    {
    }
    else
    if ((bool : false))
    {
    }
    else
    {
    }
    } |}];;

let%expect_test "if no eif or else" = run_test "def int main() { if (true) { } }"; 
  [%expect {|
    def int main()
    {
    if ((bool : true))
    {
    }
    else
    {
    }
    } |}];;

let%expect_test "int array declaration" = run_test "def int main() { int array b = [3, 4] int; }"; 
  [%expect {|
    def int main()
    {
    array b = [3, 4] int;
    } |}];;

let%expect_test "1d matrix declaration" = run_test "def int main() { int matrix m = |[[3, 4]]| int; }"; 
  [%expect {|
    def int main()
    {
    matrix m = |[[3, 4]]| int;
    } |}];;

let%expect_test "2d matrix decl" = run_test "def int main() { matrix x = |[1.2,2.3],[1.2,1.3]|; }"; 
  [%expect {|
    def int main()
    {
    matrix x = |[[1.2, 2.3],[1.2, 1.3]]| double;
    }
  |}];;

let%expect_test "lambda func decl in main" = run_test "def int main() { int a = 1; int b=1; b = (@int c { c = c*2+a*5 } b); }";
  [%expect {|
  def int main()
  {
  int a = 1;
  int b = 1;
  (int : b = (int : @ intc{ (int : c = (int : (int : (int : c) * (int : 2)) + (int : (int : a) * (int : 5)))) }));
  (int : b = (int : (int : b) + (int : a)));
  } |}];;

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

let%expect_test "Bad array entry" = 
  try run_test "def int main() { int array b = [3, 'a'] int; }" with 
    Failure e -> print_endline e;
  [%expect {|
    Illegal array entry: int = char in 'a'
    |}]

let%expect_test "Assign array to int variable" = 
  try run_test "def int main() { int array b = [3, 4] int; int c = b;}" with 
    Failure e -> print_endline e;
  [%expect {|
    Illegal bind: mismatched types: expected 'int' got 'array' instead in expr: b
    |}]
