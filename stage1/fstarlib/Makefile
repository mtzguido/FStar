include $(FSTAR_HOME)/.common.mk

FSTAR_DUNE_BUILD_OPTIONS += --no-print-directory
FSTAR_DUNE_BUILD_OPTIONS += --display=quiet

# Creates _build/default/fstar-bare/main.exe
.PHONY: fstarlib
fstarlib:
	$(call msg, "DUNE BUILD", "LIB")
	$(Q)dune build $(FSTAR_DUNE_BUILD_OPTIONS)
