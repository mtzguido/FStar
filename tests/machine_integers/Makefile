FSTAR_HOME?=../..
include $(FSTAR_HOME)/mk/common.mk
$(call need, FSTAR_EXE, asd)

include $(FSTAR_HOME)/examples/Makefile.include

.PHONY: %.run clean
.PRECIOUS: %.exe %.out


FSTAR := $(FSTAR_EXE) $(SIL) $(FSTAR_DEFAULT_ARGS)

MODULES=$(wildcard Test*.fst)

all: $(patsubst %.fst,%.run,$(MODULES)) out/BigList.memcheck

accept: $(patsubst %.fst,%.run-accept,$(MODULES))

%.exe: %.fst | out
	$(call msg, "BUILD", $(notdir $@))
	$(eval B := $(patsubst %.exe,%,$@))
	$(Q)$(FSTAR) --odir out --codegen OCaml --extract '${B}' '${B}.fst'
	$(Q)/bin/echo -e '\n\nlet _ = main ()\n' >> out/${B}.ml
	$(Q)$(OCAMLOPT) -package fstar_lib -linkpkg out/${B}.ml -o $@

%.out: %.exe
	$(call msg, "OUTPUT", $(notdir $@))
	$(Q)./$< > $@

%.run: %.out
	$(Q)diff -u --strip-trailing-cr $<.expected $<

%.run-accept: %.out
	cp $< $<.expected

out:
	mkdir -p out

# Test that this file does not use much memory (currently 580MB),
# we give it 1.5GB
out/BigList.memcheck: BigList.fst
	$(call msg, "MEMCHECK", $<)
	$(Q)( ulimit -v 1500000 && $(FSTAR) BigList.fst )
	$(Q)touch $@

clean:
	rm -rf out
	rm -f *.exe
