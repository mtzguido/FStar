# 330 - experimental features
# 333 - no hints
OTHERFLAGS+=--already_cached 'Prims FStar LowStar' --warn_error -330-333
include ../Makefile.include

NO_VERIFY=Buffers.fst

ROOTS=$(wildcard *.fst)

all: verify extraction

HAS_OCAML := $(shell which ocamlfind 2>/dev/null)

ifneq (,$(HAS_OCAML))
extraction:
	$(MAKE) -C extraction
else
extraction:
endif

.depend:
	$(FSTAR) --dep full $(ROOTS) --output_deps_to .depend

depend: .depend

include .depend

verify: $(filter-out $(addsuffix .checked,$(NO_VERIFY)),$(ALL_CHECKED_FILES))

verify-all: $(ALL_CHECKED_FILES)

%.checked:
	$(FSTAR) --cache_checked_modules $<

clean:
	rm -f .depend *.checked parsing/*.checked
	rm -rf out

.PHONY: all depend verify verify-all clean extraction

%.fsti-in %.fst-in:
	@echo --warn_error -330-274-241
