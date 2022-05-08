#!/bin/sh

make clean
make

./comma.native -l infile.test > IRoutfile.ll


llc -relocation-model=pic IRoutfile.ll > IRoutfile.s

cc -o IRoutfile.exe IRoutfile.s libstd.o

