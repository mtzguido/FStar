FSTAR:=../stage1/full/bin/fstar.exe

.PHONY: all
all: run

Hello.ml: Hello.fst Makefile
	$(FSTAR) --codegen OCaml Hello.fst --extract Hello

# OCAMLPATH=$(shell $(FSTAR) --locate_ocaml)
# FIXME
OCAMLPATH=$(CURDIR)/../../ulib-ocaml/lib

hello.exe: Hello.ml
	OCAMLPATH=$(OCAMLPATH) dune build @install
	OCAMLPATH=$(OCAMLPATH) dune install --prefix=.

.PHONY: run
run: hello.exe
	./hello.exe | grep "Hi!"
