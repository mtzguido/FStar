include $(FSTAR_HOME)/.common.mk

FSTAR_DUNE_BUILD_OPTIONS += --no-print-directory
FSTAR_DUNE_BUILD_OPTIONS += --display=quiet

# Creates _build/default/fstar-bare/main.exe
.PHONY: fstar-bare
fstar-bare: bin/fstar-bare.exe
.PHONY: bin/fstar-bare.exe
bin/fstar-bare.exe:
	$(call msg, "DUNE BUILD", "BARE")
	$(Q)dune build $(FSTAR_DUNE_BUILD_OPTIONS) --root=bare
	$(Q)dune install --root=bare --prefix=. # NB: prefix interepreted wrt to root, so it's bare/

.PHONY: fstar
fstar: bin/fstar.exe
.PHONY: bin/fstar.exe
bin/fstar.exe:
	$(call msg, "DUNE BUILD", "FULL")
	$(Q)env OCAMLPATH="$(CURDIR)/bare/lib" \
		dune build $(FSTAR_DUNE_BUILD_OPTIONS) --root=full
	$(Q)env OCAMLPATH="$(CURDIR)/bare/lib" \
		dune install --root=full --prefix=.
