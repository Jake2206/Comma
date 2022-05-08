#!/bin/bash

dockerd &> /dev/null &

RC=$(./run.sh)

eval $(opam env)

cd src

ocamlbuild -pkgs llvm llvm.analysis comma.native

make

# edit commands as needed
for TESTFILE in ./test_files/*
do 
  ./comma.native -l $TESTFILE > IRoutfile.ll
  llc -relocation-model=pic IRoutfile.ll > IRoutfile.s
  cc -o IRoutfile.exe IRoutfile.s libstd.o
  RC=$(./IRoutfile.exe)
  if [ $RC != 0 ]
  then
    echo "Executable exited with exit code $RC"
    exit 1
  fi
done

cd ..
