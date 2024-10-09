export FSTAR_HOME=$(CURDIR)
include .common.mk

.PHONY: all
all: setlink-1

FSTAR_DEFAULT_GOAL ?= full

.DEFAULT_GOAL := $(FSTAR_DEFAULT_GOAL)

### STAGES

.PHONY: stage0/bin/fstar.exe # PHONY since we cannot capture its dependencies here
stage0/bin/fstar.exe:
	$(call msg, "STAGE0")
	$(Q)mkdir -p stage0/ulib/.cache # prevent warnings
	$(MAKE) -C stage0
	ln -sf ../$@ bin/fstar-stage0.exe

.PHONY: setlink-0
setlink-0: stage0/bin/fstar.exe
	ln -sf fstar-stage0.exe bin/fstar.exe

.PHONY: 0
0: setlink-0

ifneq ($(FSTAR_EXTERNAL_STAGE0),)
FSTAR_STAGE0 := $(realpath $(FSTAR_EXTERNAL_STAGE0))
_: $(shell ln -sf $(FSTAR_STAGE0) bin/fstar-stage0.exe)
	# ^ Top-level effect, not good
endif

FSTAR_STAGE0 ?= stage0/bin/fstar.exe

FSTAR1_BARE_EXE := stage1/bare/bin/fstar.exe
FSTAR1_FULL_EXE := stage1/full/bin/fstar.exe
FSTAR2_BARE_EXE := stage2/bare/bin/fstar.exe
FSTAR2_FULL_EXE := stage2/full/bin/fstar.exe

.PHONY: $(FSTAR1_BARE_EXE)
$(FSTAR1_BARE_EXE): | $(FSTAR_STAGE0)
	$(call msg, "EXTRACT", "STAGE1 FSTARC")
	$(MAKE) -f src/fstar.mk ocaml \
		SRC=$(CURDIR)/src \
		FSTAR_BOOT=$(FSTAR_STAGE0) \
		CACHE_DIR=$(CURDIR)/stage1/fstarc.checked \
		OUTPUT_DIR=$(CURDIR)/stage1/fstarc.ml \
		CODEGEN=OCaml
	$(MAKE) -C stage1 fstar-bare
	ln -sf ../$@ bin/fstar-stage1-bare.exe

.PHONY: $(FSTAR1_FULL_EXE)
$(FSTAR1_FULL_EXE): | $(FSTAR1_BARE_EXE)
	$(call msg, "EXTRACT", "STAGE1 PLUGINS")
	$(MAKE) -f src/plugins.mk ocaml \
		SRC=$(CURDIR)/ulib \
		FSTAR_BOOT=$(CURDIR)/$(FSTAR1_BARE_EXE) \
		CACHE_DIR=$(CURDIR)/stage1/plugins.checked \
		OUTPUT_DIR=$(CURDIR)/stage1/plugins.ml \
		CODEGEN=Plugin
	$(MAKE) -C stage1 fstar
	ln -sf ../$@ bin/fstar-stage1.exe

.PHONY: setlink-1
setlink-1: $(FSTAR1_FULL_EXE)
	ln -sf fstar-stage1.exe bin/fstar.exe

.PHONY: 1
1: setlink-1

.PHONY: $(FSTAR2_BARE_EXE)
$(FSTAR2_BARE_EXE): | $(FSTAR1_FULL_EXE)
	$(call msg, "EXTRACT", "STAGE2 FSTARC")
	$(MAKE) -f src/fstar.mk ocaml \
		SRC=$(CURDIR)/src \
		FSTAR_BOOT=$(FSTAR1_FULL_EXE) \
		CACHE_DIR=$(CURDIR)/stage2/fstarc.checked \
		OUTPUT_DIR=$(CURDIR)/stage2/fstarc.ml \
		CODEGEN=OCaml
	$(MAKE) -C stage2 fstar-bare
	ln -sf ../$@ bin/fstar-stage2-bare.exe

.PHONY: $(FSTAR2_FULL_EXE)
$(FSTAR2_FULL_EXE): | $(FSTAR2_BARE_EXE)
	$(call msg, "EXTRACT", "STAGE2 PLUGINS")
	$(MAKE) -f src/plugins.mk ocaml \
		SRC=$(CURDIR)/ulib \
		FSTAR_BOOT=$(CURDIR)/$(FSTAR2_BARE_EXE) \
		CACHE_DIR=$(CURDIR)/stage2/plugins.checked \
		OUTPUT_DIR=$(CURDIR)/stage2/plugins.ml \
		CODEGEN=Plugin
	$(MAKE) -C stage2 fstar
	ln -sf ../$@ bin/fstar-stage2.exe

.PHONY: setlink-2
setlink-2: $(FSTAR2_FULL_EXE)
	ln -sf fstar-stage2.exe bin/fstar.exe

.PHONY: 2
2: setlink-2

# Stage 3 is different, we don't build it, we just check that the extracted
# OCaml files coincide exactly with stage2.

.PHONY: stage3 # PHONY since we cannot capture its dependencies here
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
	! diff -r stage2/fstarc.ml stage3/fstarc.ml

.PHONY: 3
3: check-stage3-diff

.PHONY: stage1
stage1: 1

.PHONY: stage2
stage2: 2


### LIBRARY

# Depends on some F* being there
.PHONY: lib
lib: | $(FSTAR2_FULL_EXE)
	+$(MAKE) -C ulib all

.PHONY: lib-ocaml
lib-ocaml: | lib
	+$(Q)$(MAKE) -C ulib -f Makefile.extract
	$(Q)dune build --root=ulib-ocaml

.PHONY: full
full: lib-ocaml
	$(Q)dune install --root=stage2/full --prefix=$(CURDIR)/out
	$(Q)dune install --root=ulib-ocaml --prefix=$(CURDIR)/out
	mkdir -p out/ulib
	cp ulib/*.fst $(CURDIR)/out/ulib/
	cp ulib/*.fsti $(CURDIR)/out/ulib/

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

INSTALL_EXEC := install
install_dir = cd $(1) && find . -type f -exec $(INSTALL_EXEC) -m 644 -D {} $(PREFIX)/$(2)/{} \;

.PHONY: install
install: stage2
	if [ -z "$(PREFIX)" ]; then echo "PREFIX not set" >&2; false; fi
	@# Install the binary and the binary library
	cd stage2 && dune install --profile=release --prefix=$(PREFIX)
	@# Then the standard library sources and checked files
	+$(MAKE) -C $(FSTAR_HOME)/ulib install

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
