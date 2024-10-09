FSTAR_HOME ?= ../..
include $(FSTAR_HOME)/mk/common.mk
include ../Makefile.include

all: hello testseq multi strings

hello: out Hello/Hello.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --odir out --codegen OCaml --extract 'Hello' Hello/Hello.fst
	$(OCAMLOPT) -linkpkg -package fstar_lib out/Hello.ml -o hello.exe
	./hello.exe

testseq: out TestSeq/TestSeq.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --odir out --codegen OCaml --extract 'TestSeq' TestSeq/TestSeq.fst
	$(OCAMLOPT) -linkpkg -package fstar_lib out/TestSeq.ml -o testseq.exe
	./testseq.exe

strings: out strings/TestStrings.fst
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --odir out --codegen OCaml --extract 'TestStrings' strings/TestStrings.fst
	$(OCAMLOPT) -linkpkg -package fstar_lib out/TestStrings.ml -o teststrings.exe

multi:
	$(MAKE) -C multifile

out:
	mkdir -p out

clean:
	rm -rf out *~ *.exe
