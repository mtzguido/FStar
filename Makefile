export FSTAR_HOME=$(CURDIR)
include .common.mk

FSTAR_DEFAULT_GOAL ?= full

.DEFAULT_GOAL := $(FSTAR_DEFAULT_GOAL)

### STAGES

ifneq ($(FSTAR_EXTERNAL_STAGE0),)
FSTAR0_EXE := $(realpath $(FSTAR_EXTERNAL_STAGE0))
endif

FSTAR0_EXE ?= stage0/bin/fstar.exe
FSTAR1_BARE_EXE := stage1/bare/bin/fstar.exe
FSTAR1_FULL_EXE := stage1/full/bin/fstar.exe
FSTAR2_BARE_EXE := stage2/bare/bin/fstar.exe
FSTAR2_FULL_EXE := stage2/full/bin/fstar.exe

.PHONY: 0
0 $(FSTAR0_EXE):
	$(call msg, "STAGE0")
	$(Q)mkdir -p stage0/ulib/.cache # prevent warnings
	$(MAKE) -C stage0

.PHONY: 1.bare
1.bare $(FSTAR1_BARE_EXE): $(FSTAR0_EXE)
	$(call msg, "EXTRACT", "STAGE1 FSTARC")
	$(MAKE) -f src/fstar.mk ocaml \
		SRC=$(CURDIR)/src \
		FSTAR_BOOT=$(FSTAR0_EXE) \
		CACHE_DIR=$(CURDIR)/stage1/fstarc.checked \
		OUTPUT_DIR=$(CURDIR)/stage1/fstarc.ml \
		CODEGEN=OCaml
	$(MAKE) -C stage1 fstar-bare

.PHONY: 1
1: 1.bare
1 $(FSTAR1_FULL_EXE): $(FSTAR1_BARE_EXE)
	$(call msg, "EXTRACT", "STAGE1 PLUGINS")
	$(MAKE) -f src/plugins.mk ocaml \
		SRC=$(CURDIR)/ulib \
		FSTAR_BOOT=$(CURDIR)/$(FSTAR1_BARE_EXE) \
		CACHE_DIR=$(CURDIR)/stage1/plugins.checked \
		OUTPUT_DIR=$(CURDIR)/stage1/plugins.ml \
		CODEGEN=Plugin
	$(MAKE) -C stage1 fstar

.PHONY: 2.bare
2.bare: 1
2.bare $(FSTAR2_BARE_EXE): $(FSTAR1_FULL_EXE)
	$(call msg, "EXTRACT", "STAGE2 FSTARC")
	$(MAKE) -f src/fstar.mk ocaml \
		SRC=$(CURDIR)/src \
		FSTAR_BOOT=$(FSTAR1_FULL_EXE) \
		CACHE_DIR=$(CURDIR)/stage2/fstarc.checked \
		OUTPUT_DIR=$(CURDIR)/stage2/fstarc.ml \
		CODEGEN=OCaml
	$(MAKE) -C stage2 fstar-bare

.PHONY: 2
2: 2.bare
2 $(FSTAR2_FULL_EXE): $(FSTAR2_BARE_EXE)
	$(call msg, "EXTRACT", "STAGE2 PLUGINS")
	$(MAKE) -f src/plugins.mk ocaml \
		SRC=$(CURDIR)/ulib \
		FSTAR_BOOT=$(CURDIR)/$(FSTAR2_BARE_EXE) \
		CACHE_DIR=$(CURDIR)/stage2/plugins.checked \
		OUTPUT_DIR=$(CURDIR)/stage2/plugins.ml \
		CODEGEN=Plugin
	$(MAKE) -C stage2 fstar

# Stage 3 is different, we don't build it, we just check that the
# extracted OCaml files coincide exactly with stage2. We also do not
# extract the plugins, as is stage2/fstarc and stage3/fstarc coincide,
# then they are exactly the same compiler and will extract the plugins
# in the same way.

.PHONY: stage3-bare
stage3-bare: | $(FSTAR2_FULL_EXE)
	$(call msg, "EXTRACT", "STAGE3 FSTARC")
	$(MAKE) -f src/fstar.mk ocaml \
		SRC=$(CURDIR)/src \
		FSTAR_BOOT=$(CURDIR)/$(FSTAR2_FULL_EXE) \
		CACHE_DIR=$(CURDIR)/stage3/fstarc.checked \
		OUTPUT_DIR=$(CURDIR)/stage3/fstarc.ml \
		CODEGEN=OCaml

check-stage3-diff: stage3-bare
	$(call msg, "DIFF", "STAGE2 vs STAGE3")
	@# No output expected the gitignore line
	diff -r stage2/fstarc.ml stage3/fstarc.ml

.PHONY: 3
3: check-stage3-diff

### LIBRARY

# Depends on some F* being there
.PHONY: lib
lib: $(FSTAR2_FULL_EXE)
	+$(MAKE) -C ulib all

.PHONY: lib-ocaml
lib-ocaml: lib
	+$(Q)$(MAKE) -C ulib -f Makefile.extract
	+$(Q)$(MAKE) -C ulib-ocaml ulib-ocaml

full0: lib-ocaml

full: PREFIX:=$(CURDIR)/out
full: install

.PHONY: test
test: tests examples check-stage3-diff

.PHONY: tests
tests: | lib
	+$(MAKE) -C tests all

.PHONY: examples
examples: | lib
	+$(MAKE) -C examples

.PHONY: ci
ci:
	+$(MAKE) 2
	+$(MAKE) lib
	+$(MAKE) test

.PHONY: save
save:
	if ! [ -f stage2/bin/fstar.exe ]; then echo "stage2 needs to be built first" >&2; false; fi
	rm -rf stage0/
	mkdir stage0
	cp -r stage2/extracted       stage0
	rm stage0/extracted/.gitignore # This directory is ignored in stage2, should not be in stage0
	cp -r stage2/fstar           stage0
	cp -r stage2/fstar-lib       stage0
	cp -r stage2/fstar-tests     stage0
	cp -r stage2/intfiles        stage0
	cp -r stage2/Makefile        stage0
	cp -r stage2/dune            stage0
	cp -r stage2/dune-project    stage0
	cp -r ulib                   stage0
	cp -r version.txt            stage0
	echo 'bin/' >> stage0/.gitignore
	echo 'lib/' >> stage0/.gitignore

.PHONY: install
install: lib-ocaml
	if [ -z "$(PREFIX)" ]; then echo "PREFIX not set" >&2; false; fi
	mkdir -p $(PREFIX)
	$(Q)dune install --root=stage2/full --prefix=$(shell realpath $(PREFIX))
	$(Q)dune install --root=ulib-ocaml --prefix=$(shell realpath $(PREFIX))
	mkdir -p $(PREFIX)/ulib
	cp ulib/*.fst $(PREFIX)/ulib/
	cp ulib/*.fsti $(PREFIX)/ulib/
	cp ulib/fstar.include $(PREFIX)/ulib/
	cp -r ulib/experimental $(PREFIX)/ulib/
	cp -r ulib/legacy $(PREFIX)/ulib/
	cp -r ulib/LowStar $(PREFIX)/ulib/
	cp -r ulib/.cache $(PREFIX)/ulib/

.PHONY: package
package:
	rm -rf _build
	mkdir _build
	+$(MAKE) install PREFIX=$(CURDIR)/_build/
	tar czf fstar.tar.gz -C _build .

# save:
#         $(MAKE) -C src ocaml
#         @.scripts/git-directory-untouched.sh stage0 || (echo "Stage 1 seems not clean, NOT SAVING"; false)
#         rm -rf stage1/ocaml/fstar-lib
#         cp -r  stage2/ocaml/fstar-lib stage1/ocaml/fstar-lib

# re2:
#         rm -f stage2/.depend
#         git clean -dfx stage2/ocaml
#         $(MAKE) 2


watch:
	while true; do \
		$(MAKE) ;\
		inotifywait -qre close_write,moved_to .; \
	done
