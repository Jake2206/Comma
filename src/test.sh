#!/bin/sh

make clean
make

./comma.native -l infile.test > IRoutfile.out

lli-11 -extra-archive libstd.a IRoutfile.out


