include $(FSTAR_HOME)/.common.mk
.DEFAULT_GOAL := all

.PHONY: all benchmark extra clean_checked clean fstardoc ulib-in-fsharp

# Makefile.verify has been split off because it is the actual
# Makefile that is distributed in the binary package. This
# Makefile here is not.

all: .cache
	+$(MAKE) FSTAR_HOME=$(FSTAR_HOME) -f Makefile.verify verify-all

core: .cache
	+$(MAKE) FSTAR_HOME=$(FSTAR_HOME) -f Makefile.verify verify-core

benchmark: .cache
	+$(MAKE) FSTAR_HOME=$(FSTAR_HOME) -f Makefile.verify verify-benchmark

extra: .cache
	+$(MAKE) FSTAR_HOME=$(FSTAR_HOME) -f Makefile.verify verify-extra

.cache:
	mkdir -p .cache


# Builds ulibfs.dll and the nuget package
ulib-in-fsharp: ulib-in-fsharp-dll
	+$(MAKE) -f Makefile.extract.fsharp nuget

ulib-in-fsharp-dll: core
	+$(MAKE) -f Makefile.extract.fsharp dll

.PHONY: ulib-in-fsharp-dll

clean_checked:
	$(Q)rm -f *.checked .cache/*.checked .depend

clean: clean_checked
	$(call msg, "CLEAN", "ulib/")
	$(Q)rm -f .depend.*
	$(Q)rm -f *.checked.lax .cache/*.checked.lax

INSTALL_EXEC ?= install
export INSTALL_EXEC

install: all
	+./install-ulib.sh
