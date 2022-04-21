# Comma
Etesam Ansari, ea2905
Nicholas Cheng , njd2135
Edward Sturt, ecs2220
Elysia Witham, ew2632
Andrey Uspenskiy, avu2106
Jacob Alexander, jla2206

We based our project files off of NanoC and MicroC. We also borrowed from Edward's projects. Refer to the LRM in ```doc/plt_lrm.py``` for more detailed information about the language and its implementation.

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

4: To run the tests execute the test suite with ```dune exec ./test_suite.exe``` use the command ```dune test``` from anywhere in the directory

--Other Test Stuff--

This framework is dependent on the pretty-printing functions in the ```src/ast.ml file``` and the ```src/sast.ml file```. It matches the output of the pretty print functions with the given ```expected_output``` in each test case.



--IR GENERATION WITH LLVM--
Install LLVM using 
```console
opam install llvm
```

Be sure to check the version number of LLVM (can be checked through opam, e.g. "opam show llvm"). 
There are several dependencies that might get installed alongside LLVM, and updating without checking the version can break things later on.

The top-level for Comma is represented by comma.ml -- shamelessly lifting the implementation from MicroC, comma.ml can take in flags to print the AST, SAST, or IR.
More importantly, it is the jumping off point to build the IR in the first place:

--BUILDING THE COMPILER--
Before the IR can be generated, the compiler (namely, "comma") must be built. There's probably a way to do this using Dune which we can deal with not at 5 a.m. -- for now, the way to do it is using 
```console
ocamlbuild -pkgs llvm comma.native
```

--GENERATING THE LLVM IR--
Once the compiler is built, the LLVM IR can be generated using 
```console
./comma.native -l infile.test > IRoutfile.out   
```
NOTE: the -l flag on the compiler prints to LLVM IR 

--RUNNING THE IR CODE--
Once the IR has been generated to file, it can be run using lli. 
Unfortunately, lli is finicky (which is why getting the right version of llvm is so important). The command to run it is lli-<version>, which will vary depending on the LLVM installation.
If the installed version of LLVM is 11, for example, the corresponding lli command will be lli-11. 

To execute the previously generated IR code, we pass the IR outfile to the correct lli command:
```console
lli-<version> IRoutfile.out
```