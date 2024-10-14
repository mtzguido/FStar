include $(FSTAR_ROOT)/mk/common.mk

# We cannot simply use --release as that implies `--root .`, and we are explictly
# changing it. Maybe we can just chdir instead?
FSTAR_DUNE_RELEASE_OPTIONS :=
FSTAR_DUNE_RELEASE_OPTIONS += --ignore-promoted-rules
FSTAR_DUNE_RELEASE_OPTIONS += --no-config
FSTAR_DUNE_RELEASE_OPTIONS += --profile release
FSTAR_DUNE_RELEASE_OPTIONS += --always-show-command-line
FSTAR_DUNE_RELEASE_OPTIONS += --promote-install-files
FSTAR_DUNE_RELEASE_OPTIONS += --require-dune-project-file
FSTAR_DUNE_RELEASE_OPTIONS += --ignore-lock-dir
FSTAR_DUNE_RELEASE_OPTIONS += --default-target @install

ifeq ($(V),)
FSTAR_DUNE_OPTIONS += --no-print-directory
FSTAR_DUNE_OPTIONS += --display=quiet
endif

FSTAR_DUNE_BUILD_OPTIONS := $(FSTAR_DUNE_OPTIONS)
ifeq ($(FSTAR_DUNE_RELEASE),1)
FSTAR_DUNE_BUILD_OPTIONS += $(FSTAR_DUNE_RELEASE_OPTIONS)
endif

.NOTPARALLEL:
# Sorry, but dune seems to get confused when its OCAMLPATH is changing

.PHONY: _force
_force:

fstar-bare: bare/bin/fstar.exe
bare/bin/fstar.exe: _force
	dune build $(FSTAR_DUNE_BUILD_OPTIONS) --root=bare
	dune install --root=bare --prefix=$(CURDIR)/out fstar-guts
	dune install --root=bare --prefix=$(CURDIR)/out fstarc-bare

fstar: full/bin/fstar.exe
full/bin/fstar.exe: fstar-bare _force
	env OCAMLPATH="$(CURDIR)/out/lib" \
	  dune build --root=full $(FSTAR_DUNE_BUILD_OPTIONS)
	dune install --root=full --prefix=$(CURDIR)/out
	# Install library (cp -u: don't copy unless newer)
	mkdir -p out/ulib
	cp -u -t out/ulib ulib/*.fst
	cp -u -t out/ulib ulib/*.fsti
	cp -u -t out/ulib ulib/fstar.include
	cp -u -r -t out/ulib ulib/experimental
	cp -u -r -t out/ulib ulib/legacy

fstarlib: _force
	dune build   --root=fstarlib $(FSTAR_DUNE_BUILD_OPTIONS)
	dune install --root=fstarlib --prefix=$(CURDIR)/out
	# Install checked files for the library
	mkdir -p out/ulib/.cache
	cp -u -r -t out/ulib/.cache ulib.checked/*

fstar-pluginlib: fstarlib _force
	env OCAMLPATH="$(CURDIR)/out/lib" \
	  dune build --root=fstar-pluginlib $(FSTAR_DUNE_BUILD_OPTIONS)
	dune install --root=fstar-pluginlib --prefix=$(CURDIR)/out

clean: _force
	dune clean $(FSTAR_DUNE_OPTIONS) --root=bare
	dune clean $(FSTAR_DUNE_OPTIONS) --root=full
	dune clean $(FSTAR_DUNE_OPTIONS) --root=fstarlib
	dune clean $(FSTAR_DUNE_OPTIONS) --root=fstar-pluginlib
	rm -rf inst
	rm -rf out

all: fstar fstarlib fstar-pluginlib
