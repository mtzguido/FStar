include $(FSTAR_HOME)/mk/common.mk

FSTAR_DUNE_BUILD_OPTIONS += --no-print-directory
FSTAR_DUNE_BUILD_OPTIONS += --display=quiet

.DEFAULT_GOAL := fstar

.PHONY: fstar
fstar:
	@echo "  DUNE BUILD"
	$(Q)dune build $(FSTAR_DUNE_BUILD_OPTIONS)
	@echo "  DUNE INSTALL"
	$(Q)dune install --prefix=.
