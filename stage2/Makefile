include $(FSTAR_HOME)/mk/common.mk

FSTAR_DUNE_BUILD_OPTIONS += --no-print-directory
FSTAR_DUNE_BUILD_OPTIONS += --display=quiet

.PHONY: fstar-bare
fstar-bare: bare/bin/fstar.exe
.PHONY: bare/bin/fstar.exe
bare/bin/fstar.exe:
	dune build $(FSTAR_DUNE_BUILD_OPTIONS) --root=bare
	dune install --root=bare --prefix=$(CURDIR)/inst/bare

.PHONY: fstar
fstar: full/bin/fstar.exe
.PHONY: full/bin/fstar.exe
full/bin/fstar.exe:
	env OCAMLPATH="$(CURDIR)/inst/bare/lib" \
	  dune build --root=full $(FSTAR_DUNE_BUILD_OPTIONS)
	env OCAMLPATH="$(CURDIR)/inst/bare/lib" \
	  dune install --root=full --prefix=$(CURDIR)/inst/full
	# Installed full compiler needs access to a ulib
	ln -Tsrf ../ulib inst/full/ulib

.PHONY: fstarlib
fstarlib:
	dune build   --root=fstarlib $(FSTAR_DUNE_BUILD_OPTIONS)
	dune install --root=fstarlib --prefix=$(CURDIR)/inst/fstarlib

.PHONY: fstar-pluginlib
fstar-pluginlib:
	env OCAMLPATH="$(CURDIR)/inst/fstarlib/lib:$(CURDIR)/inst/bare/lib" \
	  dune build --root=fstar-pluginlib $(FSTAR_DUNE_BUILD_OPTIONS)
