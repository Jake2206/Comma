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

Run make to construct library
```console
5. make
```

--GENERATING THE LLVM IR--
Once the compiler is built, the LLVM IR can be generated using

```console
6. ./comma.native -l infile.test > IRoutfile.ll
```

Invoke "llc" to produce a .s assembly file
```console
7. llc -relocation-model=pic IRoutfile.ll > IRoutfile.s
```

Invoke "cc" to assemble the .s file, link in libstd.o, and generate an executable
```console
8.   cc -o IRoutfile.exe IRoutfile.s libstd.o
```

NOTE: the -l flag on the compiler prints to LLVM IR
To execute the previously generated IR code, we simply execute the executable constructed from the previous step.

```console
9. ./IRoutfile.exe
```

10. Docker can be exited through
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