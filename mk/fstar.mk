include $(FSTAR_HOME)/.common.mk

FSTAR_HOME ?= ..
export FSTAR_HOME # because of the recursive calls to `make`

.SUFFIXES:
MAKEFLAGS += --no-builtin-rules

.PHONY: all
all:
	$(error src/fstar.mk: Need to specify a rule)

.PHONY: clean
clean: clean-ocaml

.PHONY: ocaml
ocaml: all-ml

# Unclear whether we even want defaults here.
# FSTAR_EXE		?= $(FSTAR_HOME)/stage0/bin/fstar.exe
# CACHE_DIR 		?= $(CACHE_DIR)/fstarc.checked
# OUTPUT_DIR	  ?= $(CACHE_DIR)/fstarc.ml
# CODEGEN       ?= OCaml
# SRC           ?= .

FSTAR_OPTIONS += $(OTHERFLAGS)
FSTAR_OPTIONS += --lax
# FSTAR_OPTIONS += --warn_error -271-241-319-274
FSTAR_OPTIONS += --cache_dir "$(CACHE_DIR)"
FSTAR_OPTIONS += --cache_checked_modules
FSTAR_OPTIONS += --odir "$(OUTPUT_DIR)"
FSTAR_OPTIONS += --include $(SRC)

FSTAR = $(FSTAR_EXE) $(SIL) $(FSTAR_OPTIONS)

# FIXME: Maintaining this list sucks. Could **the module** itself specify whether it is
# noextract? Actually, the F* compiler should already know which of its modules are
# in its library, and do this by default.
EXTRACT :=
EXTRACT += --extract '*'
EXTRACT += --extract -Prims
EXTRACT += --extract -FStar
EXTRACT += --extract -FStarC.Extraction.ML.PrintML # very much a special case

# Library wrangling
EXTRACT += --extract +FStar.Pervasives
EXTRACT += --extract -FStar.Pervasives.Native
EXTRACT += --extract +FStar.Class.Printable
EXTRACT += --extract +FStar.Seq.Base
EXTRACT += --extract +FStar.Seq.Properties

# We first lax type-check each file, producing a .checked.lax file
# We touch the file, because if F* determined that the .checked.lax
# file was already up to date, it doesn't touch it. Touching it here
# ensures that if this rule is successful then %.checked.lax is more
# recent than its dependences.
%.checked.lax:
	$(call msg, "LAXCHECK", $(basename $(basename $(notdir $@))))
	$(FSTAR) $(if $(findstring FStarC,$<),--MLish,) $<
	@# HACK: finding FStarC modules
	@touch -c $@  ## SHOULD NOT BE NEEDED

# And then, in a separate invocation, from each .checked.lax we
# extract an .ml file
%.ml:
	$(call msg, "EXTRACT", $(notdir $@))
	@# HACK we use notdir to get the module name since we need to pass in the
	@# fst (not the checked file), but we don't know where it is, so this is
	@# relying on F* looking in its include path. sigh.
	$(FSTAR) $(notdir $(subst .checked.lax,,$<)) --codegen $(CODEGEN) --extract_module $(basename $(notdir $(subst .checked.lax,,$<)))
	@touch -c $@  ## SHOULD NOT BE NEEDED

# --------------------------------------------------------------------
# Dependency analysis for bootstrapping
# --------------------------------------------------------------------

# The dependence analysis starts from the main file and the unit-tests
# file as the roots, mentioning the the modules that are to be
# extracted. This emits dependences for each of the ML files we want
# to produce.

ROOTS :=
ROOTS += $(SRC)/fstar/FStarC.Main.fst

$(CACHE_DIR)/.fstar_depend:
	$(call msg, "DEPEND")
	$(FSTAR) --dep full $(ROOTS) $(EXTRACT) --output_deps_to $@
	mkdir -p $(CACHE_DIR)

depend: $(CACHE_DIR)/.fstar_depend
include $(CACHE_DIR)/.fstar_depend

all-ml: $(ALL_ML_FILES)
