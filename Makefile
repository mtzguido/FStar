SHELL=/bin/bash
export FSTAR_ROOT=$(CURDIR)
# ^ This variable is only used by internal makefiles.
# Do NOT rely on it in client code. It is not what FSTAR_HOME was.
include mk/common.mk
undefine FSTAR_EXE # just in case

FSTAR_VERSION ?= $(shell cat version.txt)

FSTAR_DEFAULT_GOAL ?= 2
.DEFAULT_GOAL := $(FSTAR_DEFAULT_GOAL)

all-packages: package-1 package-2 package-src-1 package-src-2
all: stage3-bare all-packages

### STAGES

ifneq ($(FSTAR_EXTERNAL_STAGE0),)
FSTAR0_EXE := $(abspath $(FSTAR_EXTERNAL_STAGE0))
endif

STAGE0 ?= stage0

FSTAR0_EXE ?= $(STAGE0)/bin/fstar.exe
FSTAR1_BARE_EXE := stage1/out/bin/fstar-bare.exe
FSTAR1_FULL_EXE := stage1/out/bin/fstar.exe
FSTAR2_BARE_EXE := stage2/out/bin/fstar-bare.exe
FSTAR2_FULL_EXE := stage2/out/bin/fstar.exe

.PHONY: _force
_force:

0: $(FSTAR0_EXE)
1.bare: $(FSTAR1_BARE_EXE)
1.full: $(FSTAR1_FULL_EXE)
2.bare: $(FSTAR2_BARE_EXE)
2.full: $(FSTAR2_FULL_EXE)

# This one we assume it's rather stable, and do not
# mark it PHONY. Still adding '0' allows to force this
# build by 'make 0'.
0 $(FSTAR0_EXE):
	$(call bold_msg, "STAGE 0")
	mkdir -p $(STAGE0)/ulib/.cache # prevent warnings
	$(MAKE) -C $(STAGE0) fstar
	$(MAKE) -C $(STAGE0) trim # We don't need OCaml build files.

$(FSTAR1_BARE_EXE).src: $(FSTAR0_EXE) _force
	$(call bold_msg, "EXTRACT", "STAGE 1 FSTARC-BARE")
	env \
	  SRC=src/ \
	  FSTAR_EXE=$(FSTAR0_EXE) \
	  CACHE_DIR=stage1/fstarc.checked/ \
	  OUTPUT_DIR=stage1/fstarc.ml/ \
	  CODEGEN=OCaml \
	  TAG=fstarc \
	  $(MAKE) -f mk/fstar-01.mk ocaml

$(FSTAR1_BARE_EXE): $(FSTAR1_BARE_EXE).src _force
	$(call bold_msg, "BUILD", "STAGE 1 FSTARC-BARE")
	$(MAKE) -C stage1 fstar-bare

$(FSTAR1_FULL_EXE).src: $(FSTAR1_BARE_EXE) _force
	$(call bold_msg, "EXTRACT", "STAGE 1 PLUGINS")
	env \
	  SRC=ulib/ \
	  FSTAR_EXE=$(FSTAR1_BARE_EXE) \
	  CACHE_DIR=stage1/plugins.checked/ \
	  OUTPUT_DIR=stage1/plugins.ml/ \
	  CODEGEN=PluginNoLib \
	  OTHERFLAGS="--ext __guts $(OTHERFLAGS)" \
	  TAG=plugins \
	  $(MAKE) -f mk/plugins.mk ocaml

$(FSTAR1_FULL_EXE): $(FSTAR1_FULL_EXE).src _force
	$(call bold_msg, "BUILD", "STAGE 1 FSTARC")
	$(MAKE) -C stage1 fstar

1.alib.src: $(FSTAR1_FULL_EXE) _force
	$(call bold_msg, "EXTRACT", "STAGE 1 LIB")
	env \
	  SRC=ulib/ \
	  FSTAR_EXE=$(FSTAR1_FULL_EXE) \
	  CACHE_DIR=stage1/ulib.checked/ \
	  OUTPUT_DIR=stage1/ulib.ml/ \
	  CODEGEN=OCaml \
	  TAG=lib \
	  $(MAKE) -f mk/lib.mk all

1.alib: 1.alib.src _force
	$(call bold_msg, "BUILD", "STAGE 1 LIB")
	$(MAKE) -C stage1/ fstarlib

1.plib.src: $(FSTAR1_FULL_EXE) 1.alib.src _force
	# NB: shares .depend and checked from 1.alib.src,
	# hence the dependency, though it is not quite precise.
	$(call bold_msg, "EXTRACT", "STAGE 1 PLUGLIB")
	env \
	  SRC=ulib/ \
	  FSTAR_EXE=$(FSTAR1_FULL_EXE) \
	  CACHE_DIR=stage1/ulib.checked/ \
	  OUTPUT_DIR=stage1/ulib.pluginml/ \
	  CODEGEN=PluginNoLib \
	  TAG=pluginlib \
	  DEPFLAGS='--extract +FStar.Tactics,+FStar.Reflection,+FStar.Sealed' \
	  $(MAKE) -f mk/lib.mk all

1.plib: 1.plib.src _force | 1.alib # this last dependency only to prevent simultaneous dune builds
	$(call bold_msg, "BUILD", "STAGE 1 PLUGLIB")
	$(MAKE) -C stage1/ fstar-pluginlib

$(FSTAR2_BARE_EXE).src: $(FSTAR1_FULL_EXE) _force
	$(call bold_msg, "EXTRACT", "STAGE 2 FSTARC")
	env \
	  SRC=src/ \
	  FSTAR_EXE=$(FSTAR1_FULL_EXE) \
	  CACHE_DIR=stage2/fstarc.checked/ \
	  OUTPUT_DIR=stage2/fstarc.ml/ \
	  CODEGEN=OCaml \
	  TAG=fstarc \
	  $(MAKE) -f mk/fstar-12.mk ocaml

$(FSTAR2_BARE_EXE): $(FSTAR2_BARE_EXE).src _force
	$(call bold_msg, "BUILD", "STAGE 2 FSTARC-BARE")
	$(MAKE) -C stage2 fstar-bare FSTAR_DUNE_RELEASE=1
	# ^ Note, even if we don't release fstar-bare itself,
	# it is still part of the build of the full fstar, so
	# we set the release flag to have a more incremental build.

$(FSTAR2_FULL_EXE).src: $(FSTAR2_BARE_EXE) _force
	$(call bold_msg, "EXTRACT", "STAGE 2 PLUGINS")
	env \
	  SRC=ulib/ \
	  FSTAR_EXE=$(FSTAR2_BARE_EXE) \
	  CACHE_DIR=stage2/plugins.checked/ \
	  OUTPUT_DIR=stage2/plugins.ml/ \
	  CODEGEN=PluginNoLib \
	  OTHERFLAGS="--ext __guts $(OTHERFLAGS)" \
	  TAG=plugins \
	  $(MAKE) -f mk/plugins.mk ocaml

$(FSTAR2_FULL_EXE): $(FSTAR2_FULL_EXE).src _force
	$(call bold_msg, "BUILD", "STAGE 2 FSTARC")
	$(MAKE) -C stage2 fstar FSTAR_DUNE_RELEASE=1

2.alib.src: $(FSTAR2_FULL_EXE) _force
	$(call bold_msg, "EXTRACT", "STAGE 2 LIB")
	env \
	  SRC=ulib/ \
	  FSTAR_EXE=$(FSTAR2_FULL_EXE) \
	  CACHE_DIR=stage2/ulib.checked/ \
	  OUTPUT_DIR=stage2/ulib.ml/ \
	  CODEGEN=OCaml \
	  TAG=lib \
	  $(MAKE) -f mk/lib.mk all

2.alib: 2.alib.src _force
	$(call bold_msg, "BUILD", "STAGE 2 LIB")
	$(MAKE) -C stage2/ fstarlib FSTAR_DUNE_RELEASE=1

2.plib.src: $(FSTAR2_FULL_EXE) 2.alib.src _force
	# NB: shares .depend and checked from 2.alib.src,
	# hence the dependency, though it is not quite precise.
	$(call bold_msg, "EXTRACT", "STAGE 2 PLUGLIB")
	env \
	  SRC=ulib/ \
	  FSTAR_EXE=$(FSTAR2_FULL_EXE) \
	  CACHE_DIR=stage2/ulib.checked/ \
	  OUTPUT_DIR=stage2/ulib.pluginml/ \
	  CODEGEN=PluginNoLib \
	  TAG=pluginlib \
	  DEPFLAGS='--extract +FStar.Tactics,+FStar.Reflection,+FStar.Sealed' \
	  $(MAKE) -f mk/lib.mk all

2.plib: 2.plib.src _force | 2.alib # this last dependency only to prevent simultaneous dune builds
	$(call bold_msg, "BUILD", "STAGE 2 PLUGLIB")
	$(MAKE) -C stage2/ fstar-pluginlib FSTAR_DUNE_RELEASE=1

# Stage 3 is different, we don't build it, we just check that the
# extracted OCaml files coincide exactly with stage2. We also do not
# extract the plugins, as is stage2/fstarc and stage3/fstarc coincide,
# then they are exactly the same compiler and will extract the plugins
# in the same way.

stage3-bare: $(FSTAR2_FULL_EXE) _force
	$(call bold_msg, "EXTRACT", "STAGE 3 FSTARC")
	env \
	  SRC=src/ \
	  FSTAR_EXE=$(FSTAR2_FULL_EXE) \
	  CACHE_DIR=stage3/fstarc.checked/ \
	  OUTPUT_DIR=stage3/fstarc.ml/ \
	  CODEGEN=OCaml \
	  TAG=fstarc \
	  $(MAKE) -f mk/fstar-12.mk ocaml

stage3-diff: stage3-bare _force
	$(call bold_msg, "DIFF", "STAGE 2 vs STAGE 3")
	@# No output expected the gitignore line
	diff -r stage2/fstarc.ml stage3/fstarc.ml

3: stage3-diff

1: 1.full 1.alib 1.plib
	if [ -e out ] && ! [ -h out ]; then echo "ERROR: out/ exists and is not a symbolic link, please remove it"; false; fi
	ln -Tsf stage1/out out
	ln -Tsf stage1/out/bin/fstar.exe fstar.exe

2: 2.full 2.alib 2.plib
	if [ -e out ] && ! [ -h out ]; then echo "ERROR: out/ exists and is not a symbolic link, please remove it"; false; fi
	ln -Tsf stage2/out out
	ln -Tsf stage1/out/bin/fstar.exe fstar.exe

do-install: _force
	$(call bold_msg, "INSTALL", $(PREFIX))
	# Install fstar.exe, application library, and plugin library
	.scripts/bin-install.sh "$(BROOT)" "$(PREFIX)"

do-src-install: _force
	$(call bold_msg, "SRC INSTALL", $(PREFIX))
	# Install OCaml sources only
	.scripts/src-install.sh "$(BROOT)" "$(PREFIX)"

archive: _force
	rm -rf $(PREFIX)
	$(MAKE) do-install
	$(call bold_msg, "ARCHIVE", $(ARCHIVE))
	tar czf $(ARCHIVE) -h -C $(PREFIX) .
	rm -rf $(PREFIX)

src-archive: _force
	rm -rf $(PREFIX)
	$(MAKE) do-src-install
	$(call bold_msg, "SRC ARCHIVE", $(ARCHIVE))
	tar czf $(ARCHIVE) -h -C $(PREFIX) .
	rm -rf $(PREFIX)

package-1: 1.full 1.alib 1.plib _force
	env \
	  PREFIX=_pak1/ \
	  BROOT=stage1/ \
	  ARCHIVE=fstar-$(FSTAR_VERSION)-stage1.tar.gz \
	  $(MAKE) archive

package-src-1: $(FSTAR1_FULL_EXE).src 1.alib.src 1.plib.src _force
	env \
	  PREFIX=_srcpak1/ \
	  BROOT=stage1/ \
	  ARCHIVE=fstar-$(FSTAR_VERSION)-stage1-src.tar.gz \
	  $(MAKE) src-archive

package-2: 2.full 2.alib 2.plib _force
	env \
	  PREFIX=_pak2/ \
	  BROOT=stage2/ \
	  ARCHIVE=fstar-$(FSTAR_VERSION).tar.gz \
	  $(MAKE) archive

package-src-2: $(FSTAR2_FULL_EXE).src 2.alib.src 2.plib.src _force
	env \
	  PREFIX=_srcpak2/ \
	  BROOT=stage2/ \
	  ARCHIVE=fstar-$(FSTAR_VERSION)-src.tar.gz \
	  $(MAKE) src-archive

package: package-2
package-src: package-src-2

test: unit-tests # examples

unit-tests: FSTAR_EXE=$(abspath out/bin/fstar.exe)
unit-tests: _force
	+$(MAKE) -C tests all FSTAR_EXE=$(FSTAR_EXE)

examples: FSTAR_EXE=$(abspath out/bin/fstar.exe)
examples: _force
	#+$(MAKE) -C examples all FSTAR_EXE=$(FSTAR_EXE)

ci: _force
	+$(MAKE) 2
	+$(MAKE) lib
	+$(MAKE) test

do-save: _force
	$(call bold_msg,"SAVE", "$(FROM)  -->  $(TO)")
	rm -rf $(TO)
	mkdir -p $(TO)
	cp -r $(FROM) -T $(TO)
	rm -rf $(TO)/out
	rm -rf $(TO)/fstarc.checked
	rm -rf $(TO)/plugins.checked
	rm -rf $(TO)/ulib.checked
	dune clean --no-print-directory --display=quiet --root=$(TO)/bare
	dune clean --no-print-directory --display=quiet --root=$(TO)/full
	dune clean --no-print-directory --display=quiet --root=$(TO)/fstarlib
	dune clean --no-print-directory --display=quiet --root=$(TO)/fstar-pluginlib
	sed -i 's/a/a/' $(TO)/version.txt # hack to turn symlink into concrete file
	rm -f $(TO)/full/ulib
	rm -f $(TO)/ulib # a symlink
	cp -r ulib -T $(TO)/ulib
	# For now at least... we do not really use the stage0 F* to compile
	# normal applications, though that should definitely change if possible.
	# So, remove some more stuff.
	rm -rf $(TO)/fstarlib
	rm -rf $(TO)/fstar-pluginlib
	rm -rf $(TO)/ulib.ml
	rm -rf $(TO)/ulib.pluginml
	# We also do not ever verify anything with the stage0. So, remove
	# the hints, but this is weird...
	rm -rf $(TO)/ulib/.hints
	rm -f $(TO)/.gitignore
	echo '/out' >> $(TO)/.gitignore

save: FROM=stage2
save: TO=_new
save: do-save

bump-stage0: FROM=stage2
bump-stage0: TO=stage0
bump-stage0: do-save
	# Now that stage0 supports all features, we can return to a clean state
	# where the 01 makefile is equal to the 12 makefile. Same for stage1 support
	# and config code, we just take it from the stage2.
	rm -f mk/fstar-01.mk
	ln -s fstar-12.mk mk/fstar-01.mk
	rm -rf stage1
	cp -r stage2 stage1

# This rule brings a stage0 from an OLD fstar repo. Only useful for migrating.
bring-stage0: _force
	if [ -z "$(FROM)" ]; then echo "FROM not set" >&2; false; fi
	rm -rf stage0
	mkdir stage0
	cp -r $(FROM)/ocaml -T stage0
	ln -Tsrf mk/stage0.mk stage0/Makefile
	cp -r $(FROM)/ulib -T stage0/ulib
	find stage0/ulib -name '*.checked' -delete
	find stage0/ulib -name '*.hints' -delete
	echo '/lib' >> stage0/.gitignore

watch:
	while true; do \
	  $(MAKE) ;\
	  inotifywait -qre close_write,moved_to .; \
	done


### CLEAN

clean-depend: _force
	rm -f stage1/fstarc.checked/.*depend*
	rm -f stage1/plugins.checked/.*depend*
	rm -f stage1/ulib.checked/.*depend*
	rm -f stage2/fstarc.checked/.*depend*
	rm -f stage2/plugins.checked/.*depend*
	rm -f stage2/ulib.checked/.*depend*

clean-0: _force
	$(call bold_msg, "CLEAN", "STAGE 0")
	$(MAKE) -C $(STAGE0) clean
	rm -rf $(STAGE0)/ulib/.cache # created only to prevent warnings, always empty

clean-1: _force
	$(call bold_msg, "CLEAN", "STAGE 1")
	$(MAKE) -C stage1 clean
	rm -rf stage1/fstarc.checked
	rm -rf stage1/fstarc.ml
	rm -rf stage1/plugins.checked
	rm -rf stage1/plugins.ml
	rm -rf stage1/ulib.checked
	rm -rf stage1/ulib.ml
	rm -rf stage1/ulib.pluginml

clean-2: _force
	$(call bold_msg, "CLEAN", "STAGE 2")
	$(MAKE) -C stage2 clean
	rm -rf stage2/fstarc.checked
	rm -rf stage2/fstarc.ml
	rm -rf stage2/plugins.checked
	rm -rf stage2/plugins.ml
	rm -rf stage2/ulib.checked
	rm -rf stage2/ulib.ml
	rm -rf stage2/ulib.pluginml

clean-3: _force
	$(call bold_msg, "CLEAN", "STAGE 3")
	rm -rf stage3/

trim: clean-0 clean-1 clean-2 clean-3

clean: trim
	$(call bold_msg, "CLEAN", "out/")
	# ah.. this is just a symlink, recursive calls above should just trim
	rm -rf out

distclean: clean
	$(call bold_msg, "DISTCLEAN")
	rm -rf _new
	rm -rf _build
	rm -f fstar.tar.gz
	rm -f fstar-*.tar.gz

help:
	echo "Main rules:"
	echo "  build              build the compiler and libraries, and install it in out/"
	echo "  check              run internal tests and examples"
	echo "  package            build a binary package"
	echo "  src-archive        build an OCaml source package"
	echo "  clean              clean everything except built packages"
	echo
	echo "Optional arguments:"
	echo "  V=1                enable verbose build"
	echo "  ADMIT=1            skip verification (pass '--admit_smt_queries true')"
	echo
	echo "Rules for F* hackers:"
	echo "  all                build everything that can be built, also extract stage 3"
	echo "  0                  build the stage0 compiler (in stage0/)"
	echo "  1                  build a full stage 1 compiler and libraries + set the out/ symlink"
	echo "  2 (the default)    build a full stage 2 compiler and libraries + set the out/ symlink"
	echo "  package-1          create a binary tar.gz for the stage 1 build"
	echo "  package-2          create a binary tar.gz for the stage 2 build (= package)"
	echo "  src-archive-1      create an OCaml source distribution for the stage 1 build"
	echo "  src-archive-2      create an OCaml source distribution for the stage 2 build (= src-archive)"
	echo "  all-packages       build the four previous rules"
	echo "  clean-depend       remove all .depend files, useful when files change name"
	echo "  trim               clean some buildfiles, but retain any installed F* in out"
	echo "  distclean          remove every generated file"
	echo "  test               run the smaller unit test suite (implied by check)"
	echo "  bump-stage0        copy stage2 into stage0, and restore symlinks between stage1/stage2"
	echo "                     (essentially snapshotting a src-archive-2)"
	echo "  save               like bump-stage0, but saves the snapshot in _new/ for inspection"
	echo
	echo "You can set a different default goal by defining FSTAR_DEFAULT_GOAL in your environment."
