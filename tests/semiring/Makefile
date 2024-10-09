FSTAR_HOME?=../..
include $(FSTAR_HOME)/mk/common.mk
FSTAR=$(FSTAR_EXE) $(SIL) $(OTHERFLAGS)

# This tests that the semiring tactic can be made into a plugin.
# We should make it so in the library and just remove this, along
# with its hacks.

OCAMLOPT=OCAMLPATH="$(shell $(FSTAR_EXE) --locate_ocaml)" ocamlfind opt

FSTAR := $(FSTAR_EXE)

.PHONY: %.native %.interpreted
.PRECIOUS: %.fst.checked %.ml %.cmxs

all: CanonCommSemiring.interpreted CanonCommSemiring.native

%.fst.checked: %.fst
	$(call msg, "CHECK", $<)
	$(FSTAR) --cache_checked_modules $*.fst

%.ml: %.fst.checked
	$(call msg, "EXTRACT", $<)
	$(FSTAR) --codegen Plugin --extract $* $*.fst
	cat $*.ml.fixup >> $*.ml

%.cmxs: %.ml $(FSTAR_EXE)
	$(call msg, "OCAMLOPT", $<)
	$(OCAMLOPT) -shared -package fstar-guts -package fstar_lib -o $@ $*.ml

# REMARK: --load will compile $*.ml if $*.cmxs does not exist, but we compile it before
%.native: %.cmxs %.fst %.Test.fst
	$(call msg, "CHECK NATIVE", $<)
	$(FSTAR) --load $* $*.Test.fst
	@touch $@

%.interpreted: %.fst.checked %.Test.fst
	$(call msg, "CHECK INTERP", $<)
	$(FSTAR) $*.Test.fst
	@touch $@

clean:
	rm -f *.checked *.ml *.o *.cm[ix] *.cmxs *~
