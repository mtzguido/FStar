include $(FSTAR_HOME)/mk/common.mk

FSTAR_DUNE_BUILD_OPTIONS += --no-print-directory
FSTAR_DUNE_BUILD_OPTIONS += --display=quiet

.PHONY: fstar-bare
fstar-bare: bare/bin/fstar.exe
.PHONY: bare/bin/fstar.exe
bare/bin/fstar.exe:
	$(call msg, "DUNE BUILD", "BARE")
	dune build $(FSTAR_DUNE_BUILD_OPTIONS) --root=bare
	dune install --root=bare --prefix=$(CURDIR)/bare

.PHONY: fstar
fstar: full/bin/fstar.exe
.PHONY: full/bin/fstar.exe
full/bin/fstar.exe:
	$(call msg, "DUNE BUILD", "FULL")
	env OCAMLPATH="$(CURDIR)/bare/lib" \
	  dune build --root=full $(FSTAR_DUNE_BUILD_OPTIONS)
	env OCAMLPATH="$(CURDIR)/bare/lib" \
	  dune install --root=full --prefix=$(CURDIR)/full

.PHONY: fstarlib
fstarlib:
	$(call msg, "DUNE BUILD", "LIB")
	dune build   --root=fstarlib $(FSTAR_DUNE_BUILD_OPTIONS)
	dune install --root=fstarlib --prefix=$(CURDIR)/fstarlib

.PHONY: fstar-pluginlib
fstar-pluginlib:
	$(call msg, "DUNE BUILD", "PLUGIN LIB")
	env OCAMLPATH="$(CURDIR):$(CURDIR)/bare/:$(OCAMLPATH)" \
	  dune build --root=fstar-pluginlib $(FSTAR_DUNE_BUILD_OPTIONS)
