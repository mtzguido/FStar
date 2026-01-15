FSTAR_ROOT ?= ..
include $(FSTAR_ROOT)/mk/common.mk

ifeq ($(V),)
FSTAR_DUNE_OPTIONS += --no-print-directory
FSTAR_DUNE_OPTIONS += --display=quiet
endif

FSTAR_DUNE_BUILD_OPTIONS := $(FSTAR_DUNE_OPTIONS)
ifeq ($(FSTAR_DUNE_RELEASE),1)
FSTAR_DUNE_BUILD_OPTIONS += --release
endif

.PHONY: _force
_force:

# There can only be one dune instance running in a given project, but we
# could be asked to build several targets at once. So, wrap dune calls
# with flock, if we have it.
ifneq ($(shell which flock),)
LOCKFILE=$(CURDIR)/.fstarlock
DUNE=flock $(LOCKFILE) dune
else
# If flock is not around, at the very least disable parallelism within
# this Makefile, but external calls from the top-level Makefile could
# still pose a problem.
.NOTPARALLEL:
DUNE=dune
endif

fstarc-bare: _force
	cd dune && $(DUNE) build $(FSTAR_DUNE_BUILD_OPTIONS) fstarc-bare

tests: _force
	cd dune && $(DUNE) build $(FSTAR_DUNE_BUILD_OPTIONS) tests

fstarc-full: _force
	cd dune && $(DUNE) build $(FSTAR_DUNE_BUILD_OPTIONS) fstarc-full

libapp: _force
	cd dune && $(DUNE) build $(FSTAR_DUNE_BUILD_OPTIONS) libapp

libplugin: _force
	cd dune && $(DUNE) build $(FSTAR_DUNE_BUILD_OPTIONS) libplugin

clean: _force
	dune clean $(FSTAR_DUNE_OPTIONS) --root=dune
	rm -rf out

# In a local build, we prefer to symlink the library and checked file
# directories to get better IDE integration, but of course we cannot do
# that on actual install, and must copy all files. Note: this flag is
# also only set by the parent Makefile on Linux, since Mac's ln does not
# support the same options.
ifeq ($(FSTAR_LINK_LIBDIRS),1)
INSTALL_DIR := ln -Tsrf
else
INSTALL_DIR := cp -H -p -r
endif

# NOTE: We install ulib/ and src/ as symlinks, which is useful for
# local installs so VS Code can properly jump between these files,
# and we also avoid unnecessary copies. When building packages, we use
# tar's -h to follow and eliminate all these links.
install: PREFIX ?= $(CURDIR)/out
install: # NOTE: no deps, dune figures it out and rebuilds if needed
	@# We check for absolute so there's no confusion between the makefiles
	@# that call each other. Do NOT just use $(abspath ..) here. Also not use
	@# bashisms or 'expr' (does not work in macos)
	@# On Windows, paths may start with drive letters (C:\) instead of /
	if ! echo '$(PREFIX)' | grep -qE '^(/|[A-Za-z]:)' ; then echo "PREFIX (= $(PREFIX)) must be absolute">&2; false; fi
	@# Seems to need one final build?
	cd dune && $(DUNE) build $(FSTAR_DUNE_BUILD_OPTIONS)
	cd dune && $(DUNE) install $(FSTAR_DUNE_OPTIONS) --prefix=$(call cygpath,$(PREFIX))
	@# Install library and its checked files
	mkdir -p $(call cygpath,$(PREFIX))/lib/fstar
	rm -rf $(call cygpath,$(PREFIX))/lib/fstar/ulib
	$(INSTALL_DIR) ulib $(call cygpath,$(PREFIX))/lib/fstar/ulib
	rm -rf $(call cygpath,$(PREFIX))/lib/fstar/ulib.checked
	$(INSTALL_DIR) ulib.checked $(call cygpath,$(PREFIX))/lib/fstar/ulib.checked
	echo 'ulib'          > $(call cygpath,$(PREFIX))/lib/fstar/fstar.include
	echo 'ulib.checked' >> $(call cygpath,$(PREFIX))/lib/fstar/fstar.include
	@# Install checked files for FStarC
	rm -rf $(call cygpath,$(PREFIX))/lib/fstar/fstarc
	mkdir -p $(call cygpath,$(PREFIX))/lib/fstar/fstarc/
	$(INSTALL_DIR) $(FSTAR_ROOT)/src $(call cygpath,$(PREFIX))/lib/fstar/fstarc/src
	$(INSTALL_DIR) fstarc.checked    $(call cygpath,$(PREFIX))/lib/fstar/fstarc/src.checked
	echo 'src'          > $(call cygpath,$(PREFIX))/lib/fstar/fstarc/fstar.include
	echo 'src.checked' >> $(call cygpath,$(PREFIX))/lib/fstar/fstarc/fstar.include
	@# If we're not linking, remove the VS code configs, they have paths
	@# into the repo.
ifneq ($(FSTAR_LINK_LIBDIRS),1)
	rm -f $(call cygpath,$(PREFIX))/lib/fstar/ulib/*.fst.config.json
	rm -f $(call cygpath,$(PREFIX))/lib/fstar/fstarc/src/*.fst.config.json
endif
