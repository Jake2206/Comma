# Comma

Etesam Ansari, ea2905
Nicholas Cheng , njd2135
Edward Sturt, ecs2220
Elysia Witham, ew2632
Andrey Uspenskiy, avu2106
Jacob Alexander, jla2206

# IRGEN TO DO AND LLVM INSTALLATION GUIDE AT THE BOTTOM OF THE FILE

We based our project files off of NanoC and MicroC. We also borrowed from Edward's projects. Refer to the LRM in `doc/plt_lrm.py` for more detailed information about the language and its implementation.

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

--IR GENERATION WITH LLVM--

1. Make sure docker is running (can be installed here: https://docs.docker.com/engine/install/)

2. Type in terminal:
```console
   ./run.sh
```
3. Type in terminal:
```console
eval $(opam env)
```
Be sure to check the version number of LLVM (can be checked through opam, e.g. "opam show llvm").
There are several dependencies that might get installed alongside LLVM, and updating without checking the version can break things later on.

The top-level for Comma is represented by comma.ml -- shamelessly lifting the implementation from MicroC, comma.ml can take in flags to print the AST, SAST, or IR.
More importantly, it is the jumping off point to build the IR in the first place:

--BUILDING THE COMPILER--
Before the IR can be generated, the compiler (namely, "comma") must be built. There's probably a way to do this using Dune which we can deal with not at 5 a.m. -- for now, the way to do it is using
```console
4. ocamlbuild -pkgs llvm,llvm.analysis comma.native
````

Run make construct library
```console
5. make
````

--GENERATING THE LLVM IR--
Once the compiler is built, the LLVM IR can be generated using

```console
6. ./comma.native -l infile.test > IRoutfile.out
```

NOTE: the -l flag on the compiler prints to LLVM IR
ocamlbuild -pkgs llvm microc.native
--RUNNING THE IR CODE--
Once the IR has been generated to file, it can be run using lli.
Unfortunately, lli is finicky (which is why getting the right version of llvm is so important). The command to run it is lli-<version>, which will vary depending on the LLVM installation.
If the installed version of LLVM is 11, for example, the corresponding lli command will be lli-11.

To execute the previously generated IR code, we pass the IR outfile to the correct lli command. Option '-extra-archive' adds our standard library to the available C functions:

```console
7. lli-<version> -extra-archive <pathToLibrary> IRoutfile.out
```

8. Docker can be exited through
```console
exit
```
9. Stop docker instances by running:
``` console
docker stop $(docker ps -aq)
```

10. Terminate docker instances by running:
``` console
docker rm $(docker ps -aq)
```
# TO DO:

-- SETTING IT UP TO WORK (1-6) --

1. Set the appropriate bit values (and, I'm guessing, alignment) for our data types. Some are straightforward -- Ints are i32_t, Bools are i1_t, etc. -- but we need to be able to do that with pointers/offsets (array/matrix types); also, we have Doubles as a type, which I'm guessing can somehow be marked in IR to use the FPU instead of the ALU? I'm not sure what stage this happens at, but it has to happen somewhere.

2. The translate function takes in the SAST.sprogram and converts it to LLVM code -- the issue is we can't copy it directly from MicroC because their programs are tuples of bind list \* sfunc_def list; ours is a struct {
   slocals: bind list;
   sbody: sstmt list;
   sfunctions: sfunc_def list;
   }. So, basically, when we try to pass any of the arguments to subsequent functions, we have to adjust them accordingly. I'm guessing this means we have to access individual fields of the struct and pass them as arguments, if we want to keep the existing structure of the LLVM (which would probably be easier than trying to write the whole thing from scratch).

3. Directly related to above -- global_vars. (Are we even allowing variables to be declared outside functions?) In any case, in MicroC, global_vars creates the top-level lookup table with whatever it finds from the first argument above ("bind list" in the tuple). Depending on if we're allowed to have global variables, we should either figure out how to do this... or, if there can't be anything outside main(), then maybe the top level lookup table should be the locals (where the function is "main")?

4. Hooking C functions (or functions available through LLVM). Might make our lives easier when it comes to doing actual computation.
   An example:

let printf_t : L.lltype =
L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
let printf_func : L.llvalue =
L.declare_function "printf" printf_t the_module

links the printf stdlib function to the program, as far as I can tell. It is invoked in build_expr with  
 .....
| SCall ("print", [e]) ->
L.build_call printf_func [| int_format_str ; (build_expr builder e) |]
"printf" builder

If we want to use other functionality -- we probably have to use this format or something similar.

5. Function declarations is where it gets REALLY confusing (in case you weren't confused yet). I'm pretty sure we don't want to mess with the return types of each function since that could potentially REALLY mess up what LLVM takes in. I tried experimenting a little, and made things considerably worse (you might get a function to compile, but the rest of the irgen will break in the process). This means we have to deal with the same problem as in 3: parsing the right args out in the right format. In theory, it just wants an fdecl -- which is a sfunc_def -- and whatever the hell "m" is (from the globals section above). In practice, we must've renamed something or moved it around, because it keeps complaining.

6. "Build funcion body" and "local vars" shouldn't be too bad if we figure out how to do the stuff above. I hope. It looks like they basically follow the same format as the stuff before -- but there's also the declaration of "formals" which might things more complicated.

-- THE BIG PARTS (7,8) -- 7) Build_expr is the meat of irgen. Fortunately, it looks like if we can figure out the stuff earlier (and parse out the proper types for everything), the MicroC version provides a decently solid framework. Matching SCall with linked C / LLVM functions lets you call pre-existing stuff. Otherwise, it's pretty straightforward. But we have to populate it with our own types/functions/operators etc.

8. Blocks! Again, the good news is, hopefully, if we make it this far, this means that we've managed to parse out what needs to be parsed. This means that we can mimic the existing stuff -- if we try to implement a "for" loop, for example, we can steal the block structure from "while". (How we manage/alter the predicate I have no idea, but that's another story; the block strucutre is really similar -- append the "for", branch into the for block, create a temporary builder inside it, branch to the end block.) This is going to be really hard to actually implement, isn't it?

For the parts above, I didn't list individual tasks (maybe that's stupid), because I assume if we've made it this far, we have a much better idea of how everything works.
But, obviously, we need to be able to support everything in our SAST, so that might be a good starting point. If it's got an "S" in front of it (SBinop, SIf, whatever), it needs to have a match in irgen.
