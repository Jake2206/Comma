# Comma

We based our project off of NanoC and MicroC. We also borrowed from Edward's projects.

--DUNE INSTALLATION--
Install opam: https://opam.ocaml.org/doc/Install.html

Install dune: after opam is installed use the command 

``` console
opam install dune
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

"Hello World!" will print to the terminal

For a quick dune walkthrough check out:
https://medium.com/@bobbypriambodo/starting-an-ocaml-app-project-using-dune-d4f74e291de8

--Test Suite--

1: There are two types of tests, tests expected to pass and tests expected to fail

2: There is a function for each type of test. ```run_pass_test``` and ```run_fail_test```

3: There is a list of test cases called parse_tests. To add a test case to the list you must follow this syntax:

```<test_case_function> <test_name_string> <test_instance_string> <test_expected_result_string>```

for example: 
``` console
run_pass_test "int dclr" "int a;" "int a;\n"
```

4: To run the tests use the command ```dune test``` from anywhere in the directory

--Other Test Stuff--

1: There are some tests that are commented out until some more functionality has been added (so that there aren't tons of failed tests).

2:This framework is dependent on the pretty-printing functions in the ```src/ast.ml file```. It matches the output of the pretty print functions with the given ```expected_output``` in each test case. As we build more functionality and expand the pretty printing functions, if we use this testing framework, we will need to make sure that the pretty printing output and the expected output match.
