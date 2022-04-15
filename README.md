# Comma
Etesam Ansari, ea2905
Nicholas Cheng , njd2135
Edward Sturt, ecs2220
Elysia Witham, ew2632
Andrey Uspenskiy, avu2106
Jacob Alexander, jla2206

We based our project files off of NanoC and MicroC. We also borrowed from Edward's projects. Refer to the LRM in ```doc/plt_lrm.py``` for more detailed information about the language and its implementation.

--ppx INSTALLATION--
``` console
opam install ppx_expect
```

--BUILD AND EXECUTE--
To build use the command 

```console
dune build <file path/name.exe>
```

To execute use the command: 
```console
dune exec <file path/name.exe>
```

For example if you type:
```console
dune build bin/main.exe
dune exec bin/main.exe
```

the main.ml file will be run


--Test Suite--

1: There are two types of tests, tests expected to pass and tests expected to fail

2: To run the tests use the command ```dune test```

3: To create a new test that should pass use the following syntax:
```
let%expect_test "int decl w/ assign" = run_test "def int main() { int b = 3; }"; 
  [%expect {||}]
```
Then you need to run the test suite on the command line ```dune test```
The expected result will print out to the terminal.
You can then take the expected result and write it into the test, in this example resulting in:
```
let%expect_test "int decl w/ assign" = run_test "def int main() { int b = 3; }"; 
  [%expect {|
    def int main()
    {
    int b = 3;
    }
    |}]
```
4: To create a test that will fail use the same test and see technique as a pass test but with the following syntax:
```
let%expect_test "Bad character" = 
  try run_test "def int main() { int b = &; }" with 
    Failure e -> print_endline e;
  [%expect {|
    illegal character &
    |}]
```
Note that for our custom exceptions we use a generic ```Failure e``` in the with block, but for ocaml exceptions we need to use the specific error that is returned by the test, for example 
```
let%expect_test "int decl w/ illegal var name" = 
  try run_test "def int main() { int double; }" with 
    Parsing.Parse_error -> print_endline "Parsing.Parse_error";
  [%expect {|
    Parsing.Parse_error
    |}]
```

For more examples look in the ```tests/test_suite.ml``` file.

--Other Test Stuff--

This framework is dependent on the pretty-printing functions in the ```src/ast.ml file``` and the ```src/sast.ml file```.
