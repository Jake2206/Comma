
default: comma.native libstd.a

comma.native: irgen.ml
	ocamlbuild -pkgs llvm,llvm.analysis comma.native

libstd.a: libstd.o
	ar -crs libstd.a libstd.o 
	ranlib libstd.a

libstd.o: libstd.c libstd.h 


.PHONY: clean
clean:
	rm -f comma.native
	rm -rf *.o *.a
