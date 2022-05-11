#!/bin/bash

#dockerd &> /dev/null &

#RC=$(./run.sh)

#eval $(opam env)

cd src

make clean
ocamlbuild -pkgs llvm,llvm.analysis comma.native
make

# edit commands as needed
for TESTFILE in ./test_files/*
do 
  echo "TEST RESULT:"
  ./comma.native -l $TESTFILE > IRoutfile.ll
  llc -relocation-model=pic IRoutfile.ll > IRoutfile.s
  cc -o IRoutfile.exe IRoutfile.s libstd.o
  ./IRoutfile.exe
  echo "PASSED"
done

cd ..
