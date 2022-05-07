# Comma

Etesam Ansari, ea2905
Nicholas Cheng , njd2135
Edward Sturt, ecs2220
Elysia Witham, ew2632
Andrey Uspenskiy, avu2106
Jacob Alexander, jla2206

We based our project files off of NanoC and MicroC. We also borrowed from Edward's projects. Refer to the LRM in `doc/plt_lrm.py` for more detailed information about the language and its implementation.

--Test Suite--

We no longer have a functioning test suite after updating to docker.

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

The top-level for Comma is represented by comma.ml -- shamelessly lifting the implementation from MicroC, comma.ml can take in flags to print the AST, SAST, or IR.
More importantly, it is the jumping off point to build the IR in the first place:

--BUILDING THE COMPILER--
Before the IR can be generated, the compiler (namely, "comma") must be built.
```console
4. ocamlbuild -pkgs llvm,llvm.analysis comma.native
````

--GENERATING THE LLVM IR--
Once the compiler is built, the LLVM IR can be generated using

```console
5. ./comma.native -l infile.test > IRoutfile.out
```

NOTE: the -l flag on the compiler prints to LLVM IR

--RUNNING THE IR CODE--
Once the IR has been generated to file, it can be run using lli. Ensure you are running llvm version 11.
To execute the previously generated IR code, we pass the IR outfile to the correct lli command:

```console
6. lli-11 IRoutfile.out
```

7. Docker can be exited through
```console
exit
```
8. Stop docker instances by running:
``` console
docker stop $(docker ps -aq)
```

9. Terminate docker instances by running:
``` console
docker rm $(docker ps -aq)
```
# TO DO:

-- SETTING IT UP TO WORK (1-6) --

1. Hooking C functions (or functions available through LLVM). Might make our lives easier when it comes to doing actual computation.
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