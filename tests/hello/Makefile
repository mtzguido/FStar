FSTAR:=../../out/bin/fstar.exe

.PHONY: all
all: run

Hello.ml: Hello.fst # Makefile
	$(FSTAR) --codegen OCaml Hello.fst --extract Hello

OCAMLPATH=$(shell $(FSTAR) --locate_ocaml)

bin/hello.exe: Hello.ml
	OCAMLPATH=$(OCAMLPATH) dune build @install --profile=release
	OCAMLPATH=$(OCAMLPATH) dune install --prefix=.

.PHONY: run
run: hello.exe
	./bin/hello.exe | grep "Hi!"
