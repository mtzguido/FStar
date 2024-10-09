FSTAR_EXE ?= ../../out2/bin/fstar.exe

.PHONY: all
all: run

Hello.ml: Hello.fst # Makefile
	$(FSTAR_EXE) --codegen OCaml Hello.fst --extract Hello

OCAMLPATH=$(shell $(FSTAR_EXE) --locate_ocaml)

bin/hello.exe: Hello.ml
	OCAMLPATH=$(OCAMLPATH) dune build @install --profile=release
	OCAMLPATH=$(OCAMLPATH) dune install --prefix=.

.PHONY: run
run: bin/hello.exe
	./bin/hello.exe | grep "Hi!"
