FSTAR_HOME ?= ../..
include $(FSTAR_HOME)/mk/common.mk

FSTAR=$(FSTAR_EXE) $(SIL) $(OTHERFLAGS)

# Tests for which the native tactics used in module named Sample.Test.fst are
# declared in a corresponding module named Sample.fst
TAC_MODULES=Print\
    Split\
    BV\
    UnitTests\
    Bane\
    Canon\
    Simple\
    SimpleTactic\
    Evens\
    Embeddings\
    Plugins\
    Registers.List\
    Sealed.Plugins \
		LocalState

# Tests for which the native tatics are declared and used in the same module
ALL=Apply\
    Arith\
    CanonDeep\
    Clear\
    Cut\
    DependentSynth\
    Fail\
    Imp\
    Logic\
    Nest\
    Pruning\
    Rename\
    Retype\
    Sequences\
    Tutorial\
    Unify

# FIXME: Add back
    # Simplifier\

OTHERFLAGS +=

all: $(addsuffix .sep.test, $(TAC_MODULES)) $(addsuffix .test, $(ALL))

# .depend:
# 	$(FSTAR) --dep full $(addsuffix .Test.fst, $(ALL)) --output_deps_to .depend

# include .depend

.PHONY: %.test
.PRECIOUS: %.ml

%.test: %.fst %.ml
	$(call msg, "CHECK", $<)
	$(FSTAR) $*.fst --load $*
	touch -c $@

%.sep.test: %.fst %.ml
	$(call msg, "CHECK", $<)
	$(FSTAR) $*.Test.fst --load $*
	touch -c $@

%.ml: %.fst
	$(call msg, "EXTRACT", $<)
	$(FSTAR) $*.fst --cache_checked_modules --codegen Plugin --extract $*
	touch $@

%.clean:
	rm -f Registers_List.ml Registers.List.ml Registers_List.cmxs

%.native: %.fst Registers.List.ml
	$(call msg, "NATIVE", $<)
	$(FSTAR) $*.fst --load Registers.List --warn_error -266

%.interp: %.fst Registers.List.fst
	$(call msg, "INTERP", $<)
	$(FSTAR) $*.fst


clean:
	rm -f *.test *.ml *.o *.cm[ix] *.cmxs
