.PHONY: test doc

build:
	dune build

clean:
	dune clean

code:
	-dune build
	code .
	! dune build --watch

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe 

start:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc:
	dune build @doc 

opendoc: doc
	@bash opendoc.sh


