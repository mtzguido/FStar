export FSTAR_HOME=$(CURDIR)
include mk/common.mk

FSTAR_DEFAULT_GOAL ?= 2
.DEFAULT_GOAL := $(FSTAR_DEFAULT_GOAL)

### STAGES

ifneq ($(FSTAR_EXTERNAL_STAGE0),)
FSTAR0_EXE := $(realpath $(FSTAR_EXTERNAL_STAGE0))
endif

FSTAR0_EXE ?= stage0/bin/fstar.exe
FSTAR1_BARE_EXE := stage1/inst/bare/bin/fstar.exe
FSTAR1_FULL_EXE := stage1/inst/full/bin/fstar.exe
FSTAR2_BARE_EXE := stage2/inst/bare/bin/fstar.exe
FSTAR2_FULL_EXE := stage2/inst/full/bin/fstar.exe

.PHONY: 0
.PHONY: 1.bare 1.full 1.lib 1
.PHONY: 2.bare 2.full 2.lib 2
0: $(FSTAR0_EXE)
1.bare: $(FSTAR1_BARE_EXE)
1.full: $(FSTAR1_FULL_EXE)
2.bare: $(FSTAR2_BARE_EXE)
2.full: $(FSTAR2_FULL_EXE)

# This one we assume it's rather stable, and do not
# mark it PHONY. Still adding '0' allows to force this
# build by 'make 0'.
0 $(FSTAR0_EXE):
	$(call msg, "STAGE0")
	mkdir -p stage0/ulib/.cache # prevent warnings
	$(MAKE) -C stage0

.PHONY: $(FSTAR1_BARE_EXE)
$(FSTAR1_BARE_EXE): $(FSTAR0_EXE)
	$(call msg, "EXTRACT", "STAGE1 FSTARC-BARE")
	$(MAKE) -f mk/fstar.mk ocaml \
	  SRC=$(CURDIR)/src \
	  FSTAR_EXE=$(FSTAR0_EXE) \
	  CACHE_DIR=$(CURDIR)/stage1/fstarc.checked \
	  OUTPUT_DIR=$(CURDIR)/stage1/fstarc.ml \
	  CODEGEN=OCaml
	$(call msg, "BUILD", "STAGE1 FSTARC-BARE")
	$(MAKE) -C stage1 fstar-bare

.PHONY: $(FSTAR1_FULL_EXE)
$(FSTAR1_FULL_EXE): $(FSTAR1_BARE_EXE)
	$(call msg, "EXTRACT", "STAGE1 PLUGINS")
	$(MAKE) -f mk/plugins.mk ocaml \
	  SRC=$(CURDIR)/ulib \
	  FSTAR_EXE=$(CURDIR)/$(FSTAR1_BARE_EXE) \
	  CACHE_DIR=$(CURDIR)/stage1/plugins.checked \
	  OUTPUT_DIR=$(CURDIR)/stage1/plugins.ml \
	  CODEGEN=PluginNoLib
	$(call msg, "BUILD", "STAGE1 FSTARC")
	$(MAKE) -C stage1 fstar

.PHONY: $(FSTAR2_BARE_EXE)
$(FSTAR2_BARE_EXE): $(FSTAR1_FULL_EXE)
	$(call msg, "EXTRACT", "STAGE2 FSTARC")
	$(MAKE) -f mk/fstar.mk ocaml \
	  SRC=$(CURDIR)/src \
	  FSTAR_EXE=$(FSTAR1_FULL_EXE) \
	  CACHE_DIR=$(CURDIR)/stage2/fstarc.checked \
	  OUTPUT_DIR=$(CURDIR)/stage2/fstarc.ml \
	  CODEGEN=OCaml
	$(call msg, "BUILD", "STAGE2 FSTARC-BARE")
	$(MAKE) -C stage2 fstar-bare

.PHONY: $(FSTAR2_FULL_EXE)
$(FSTAR2_FULL_EXE): $(FSTAR2_BARE_EXE)
	$(call msg, "EXTRACT", "STAGE2 PLUGINS")
	$(MAKE) -f mk/plugins.mk ocaml \
	  SRC=$(CURDIR)/ulib \
	  FSTAR_EXE=$(CURDIR)/$(FSTAR2_BARE_EXE) \
	  CACHE_DIR=$(CURDIR)/stage2/plugins.checked \
	  OUTPUT_DIR=$(CURDIR)/stage2/plugins.ml \
	  CODEGEN=PluginNoLib
	$(call msg, "BUILD", "STAGE2 FSTARC")
	$(MAKE) -C stage2 fstar

# Stage 3 is different, we don't build it, we just check that the
# extracted OCaml files coincide exactly with stage2. We also do not
# extract the plugins, as is stage2/fstarc and stage3/fstarc coincide,
# then they are exactly the same compiler and will extract the plugins
# in the same way.

.PHONY: stage3-bare
stage3-bare: | $(FSTAR2_FULL_EXE)
	$(call msg, "EXTRACT", "STAGE3 FSTARC")
	$(MAKE) -f mk/fstar.mk ocaml \
	  SRC=$(CURDIR)/src \
	  FSTAR_EXE=$(CURDIR)/$(FSTAR2_FULL_EXE) \
	  CACHE_DIR=$(CURDIR)/stage3/fstarc.checked \
	  OUTPUT_DIR=$(CURDIR)/stage3/fstarc.ml \
	  CODEGEN=OCaml

check-stage3-diff: stage3-bare
	$(call msg, "DIFF", "STAGE2 STAGE3")
	@# No output expected the gitignore line
	diff -r stage2/fstarc.ml stage3/fstarc.ml

.PHONY: 3
3: check-stage3-diff

### LIBRARY

.PHONY: 1.lib
1.lib: $(FSTAR1_FULL_EXE)
	$(call msg, "EXTRACT", "STAGE1 LIB")
	mkdir -p stage1/ulib.checked # stupid
	mkdir -p stage1/ulib.ml # stupid
	+$(MAKE) -f mk/lib.mk all \
	  SRC=$(CURDIR)/ulib \
	  FSTAR_EXE=$(CURDIR)/$(FSTAR1_FULL_EXE) \
	  CACHE_DIR=$(CURDIR)/stage1/ulib.checked \
	  OUTPUT_DIR=$(CURDIR)/stage1/ulib.ml \
	  CODEGEN=OCaml \
	  TAG=lib
	$(call msg, "BUILD", "STAGE1 LIB")
	+$(MAKE) -C stage1/ fstarlib

.PHONY: 1.plib
1.plib: $(FSTAR1_FULL_EXE) 1.lib
	# NB: shares .depend and checked from 1.lib,
	# hence the dependency, though it is not quite precise.
	$(call msg, "EXTRACT", "STAGE1 PLUGLIB")
	mkdir -p stage1/ulib.checked # stupid
	mkdir -p stage1/ulib.pluginml # stupid
	+$(MAKE) -f mk/lib.mk all \
	  SRC=$(CURDIR)/ulib \
	  FSTAR_EXE=$(CURDIR)/$(FSTAR1_FULL_EXE) \
	  CACHE_DIR=$(CURDIR)/stage1/ulib.checked \
	  OUTPUT_DIR=$(CURDIR)/stage1/ulib.pluginml \
	  CODEGEN=PluginNoLib \
	  TAG=pluginlib \
	  DEPFLAGS='--extract +FStar.Tactics,+FStar.Reflection,+FStar.Sealed'
	$(call msg, "BUILD", "STAGE1 PLUGLIB")
	+$(MAKE) -C stage1/ fstar-pluginlib

.PHONY: 2.lib
2.lib: $(FSTAR2_FULL_EXE)
	$(call msg, "EXTRACT", "STAGE2 LIB")
	mkdir -p stage2/ulib.checked # stupid
	mkdir -p stage2/ulib.ml # stupid
	+$(MAKE) -f mk/lib.mk all \
	  SRC=$(CURDIR)/ulib \
	  FSTAR_EXE=$(CURDIR)/$(FSTAR2_FULL_EXE) \
	  CACHE_DIR=$(CURDIR)/stage2/ulib.checked \
	  OUTPUT_DIR=$(CURDIR)/stage2/ulib.ml \
	  CODEGEN=OCaml \
	  TAG=lib
	$(call msg, "BUILD", "STAGE2 LIB")
	+$(MAKE) -C stage2/ fstarlib

.PHONY: 2.plib
2.plib: $(FSTAR2_FULL_EXE) 2.lib
	# NB: shares .depend and checked from 2.lib,
	# hence the dependency, though it is not quite precise.
	$(call msg, "EXTRACT", "STAGE2 PLUGLIB")
	mkdir -p stage2/ulib.checked # stupid
	mkdir -p stage2/ulib.pluginml # stupid
	+$(MAKE) -f mk/lib.mk all \
	  SRC=$(CURDIR)/ulib \
	  FSTAR_EXE=$(CURDIR)/$(FSTAR2_FULL_EXE) \
	  CACHE_DIR=$(CURDIR)/stage2/ulib.checked \
	  OUTPUT_DIR=$(CURDIR)/stage2/ulib.pluginml \
	  CODEGEN=PluginNoLib \
	  TAG=pluginlib \
	  DEPFLAGS='--extract +FStar.Tactics,+FStar.Reflection,+FStar.Sealed'
	$(call msg, "BUILD", "STAGE2 PLUGLIB")
	+$(MAKE) -C stage2/ fstar-pluginlib

1: export PREFIX=$(CURDIR)/stage1/out
1: export BROOT=$(CURDIR)/stage1
1: 1.full 1.lib 1.plib
	$(MAKE) do-install
	ln -Tsf stage1/out out

2: export PREFIX=$(CURDIR)/stage2/out
2: export BROOT=$(CURDIR)/stage2
2: 2.full 2.lib 2.plib
	$(MAKE) do-install
	ln -Tsf stage2/out out

.PHONY: do-install
do-install:
	if [ -z "$(PREFIX)" ]; then echo "PREFIX not set" >&2; false; fi
	if [ -z "$(BROOT)" ]; then echo "BROOT not set" >&2; false; fi
	$(call msg, "INSTALL", $(PREFIX))
	mkdir -p $(PREFIX)
	# Install fstar.exe, application library, and plugin library
	dune install --prefix=$(abspath $(PREFIX)) --root=$(BROOT)/full
	dune install --prefix=$(abspath $(PREFIX)) --root=$(BROOT)/fstarlib
	dune install --prefix=$(abspath $(PREFIX)) --root=$(BROOT)/bare fstar-guts
	dune install --prefix=$(abspath $(PREFIX)) --root=$(BROOT)/fstar-pluginlib
	# Install library with its checked files.
	mkdir -p $(PREFIX)/ulib
	cp -t $(PREFIX)/ulib ulib/*.fst
	cp -t $(PREFIX)/ulib ulib/*.fsti
	cp -t $(PREFIX)/ulib ulib/fstar.include
	cp -r -t $(PREFIX)/ulib ulib/experimental
	cp -r -t $(PREFIX)/ulib ulib/legacy
	cp -r -t $(PREFIX)/ulib ulib/LowStar
	mkdir -p $(PREFIX)/ulib/.cache
	cp -r -t $(PREFIX)/ulib/.cache $(BROOT)/ulib.checked/*

.PHONY: archive
archive:
	rm -rf $(PREFIX)
	mkdir $(PREFIX)
	$(MAKE) do-install
	$(call msg, "ARCHIVE", $(ARCHIVE))
	tar czf $(ARCHIVE) -C $(PREFIX) .
	rm -rf $(PREFIX)

package-1: export PREFIX=$(CURDIR)/_build
package-1: export BROOT=$(CURDIR)/stage1
package-1: export ARCHIVE=fstar-$(shell cat version.txt)-stage1.tar.gz
package-1: 1.full 1.lib 1.plib
	$(MAKE) archive
	ln -Tsrf $(ARCHIVE) fstar.tar.gz

package-2: export PREFIX=$(CURDIR)/_build
package-2: export BROOT=$(CURDIR)/stage2
package-2: export ARCHIVE=fstar-$(shell cat version.txt).tar.gz
package-2: 2.full 2.lib 2.plib
	$(MAKE) archive
	ln -Tsrf $(ARCHIVE) fstar.tar.gz

package: package-2

.PHONY: test-1
test-1: FSTAR_EXE=$(CURDIR)/stage1/out/bin/fstar.exe
test-1: tests examples

.PHONY: test-2
test-2: FSTAR_EXE=$(CURDIR)/stage2/out/bin/fstar.exe
test-2: tests examples

.PHONY: test
test: tests examples

.PHONY: tests
tests: FSTAR_EXE?=$(CURDIR)/out/bin/fstar.exe
tests:
	+$(MAKE) -C tests all FSTAR_EXE=$(FSTAR_EXE)

.PHONY: examples
examples: FSTAR_EXE?=$(CURDIR)/out/bin/fstar.exe
examples:
	+$(MAKE) -C examples all FSTAR_EXE=$(FSTAR_EXE)

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

watch:
	while true; do \
	  $(MAKE) ;\
	  inotifywait -qre close_write,moved_to .; \
	done
