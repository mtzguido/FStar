FSTAR_HOME ?= ../..
include $(FSTAR_HOME)/examples/Makefile.include

all: inline_let all_cmi Eta_expand.test Div.test ExtractAs.test

%.exe: %.fst | out
	$(FSTAR) $(FSTAR_DEFAULT_ARGS) --odir out --codegen OCaml $<
	$(OCAMLOPT) -package fstar_lib -linkpkg out/$(patsubst %.fst,%.ml,$<) -o $@

%.test: %.exe
	$(call msg,RUN,$<)
	$(Q)./$<
	$(Q)touch $@

Div.test: Div.exe
	$(call msg,TIMEOUT,$<)
	$(Q)timeout 1 ./Div.exe ; \
	  RC=$$? ;\
	  if ! [ $$RC -eq 124 ]; then echo "ERROR: Div.exe terminated!?!?!"; false; fi
	$(Q)touch $@

inline_let: InlineLet.fst
	$(FSTAR) --codegen OCaml InlineLet.fst
	egrep -A 10 test InlineLet.ml | grep -qs "17"
	@touch $@

all_cmi:
	+$(MAKE) -C cmi all

out:
	mkdir -p out

clean:
	rm -rf out
	rm -f *.exe
	rm -f *~
	rm -f *.exe *.test
	rm -f inline_let

