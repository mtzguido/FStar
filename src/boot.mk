include ../.common.mk

FSTAR_HOME ?= ..
export FSTAR_HOME # because of the recursive calls to `make`

.SUFFIXES:
MAKEFLAGS += --no-builtin-rules

.PHONY: all
all:
	$(error src/Makefile: Need to specify a rule)

.PHONY: clean
clean: clean-ocaml

.PHONY: ocaml
ocaml: all-ml

FSTAR_BOOT		?= $(FSTAR_HOME)/stage0/bin/fstar.exe
ROOT_OUT		?= $(FSTAR_HOME)/stage1
CODEGEN     ?= Plugin

CACHE_DIR 		:= $(ROOT_OUT)/checked/
OUTPUT_DIRECTORY	:= $(ROOT_OUT)/extracted/

FSTAR_BOOT_OPTIONS += $(OTHERFLAGS)
FSTAR_BOOT_OPTIONS += --lax
# FSTAR_BOOT_OPTIONS += --warn_error -271-241-319-274
FSTAR_BOOT_OPTIONS += --cache_dir "$(CACHE_DIR)"
FSTAR_BOOT_OPTIONS += --cache_checked_modules
FSTAR_BOOT_OPTIONS += --odir "$(OUTPUT_DIRECTORY)"

# FIXME: This should be somehow fixed better. We should distinguish the
# namespaces of the 'old' fstarlib and the new one.
# FSTAR_BOOT_OPTIONS += --no_default_includes
FSTAR_BOOT_OPTIONS += --include $(realpath ../ulib/)

FSTAR = $(RUNLIM) $(FSTAR_BOOT) $(SIL) $(FSTAR_BOOT_OPTIONS)

# FIXME: Maintaining this list sucks. Could **the module** itself specify whether it is
# noextract?
EXTRACT = --extract '*'					\
  --extract -Prims					\
  --extract -FStar.Pervasives.Native			\
  --extract -FStar.All					\
  --extract -FStar.Bytes				\
  --extract -FStar.Char					\
  --extract -FStarC.CommonST				\
  --extract -FStarC.Compiler.Effect			\
  --extract -FStarC.Compiler.List			\
  --extract -FStarC.Dyn					\
  --extract -FStar.Exn					\
  --extract -FStarC.Extraction.ML.PrintML		\
  --extract -FStar.Float				\
  --extract -FStar.Ghost				\
  --extract -FStar.Heap					\
  --extract -FStar.Int16				\
  --extract -FStar.Int32				\
  --extract -FStar.Int64				\
  --extract -FStar.Int8					\
  --extract +FStar.Int.Cast.Full			\
  --extract -FStar.List					\
  --extract +FStar.List.Pure.Base			\
  --extract +FStar.List.Tot.Properties			\
  --extract -FStar.Monotonic.Heap			\
  --extract -FStar.HyperStack.ST			\
  --extract -FStar.Option				\
  --extract -FStar.Printf				\
  --extract -FStar.Range				\
  --extract -FStar.ST					\
  --extract -FStar.String				\
  --extract -FStarC.Tactics.Load				\
  --extract -FStarC.Tactics.Native			\
  --extract -FStar.TSet					\
  --extract -FStar.UInt16				\
  --extract -FStar.UInt32				\
  --extract -FStar.UInt64				\
  --extract -FStar.UInt8				\
  --extract -FStar.Util					\
  --extract -FStar.Version

# We first lax type-check each file, producing a .checked.lax file
# We touch the file, because if F* determined that the .checked.lax
# file was already up to date, it doesn't touch it. Touching it here
# ensures that if this rule is successful then %.checked.lax is more
# recent than its dependences.
%.checked.lax:
	$(call msg, "LAXCHECK", $(basename $(basename $(notdir $@))))
	$(Q)$(FSTAR) $(if $(findstring /ulib/,$<),,--MLish) $<
	$(Q)@touch -c $@  ## SHOULD NOT BE NEEDED

# And then, in a separate invocation, from each .checked.lax we
# extract an .ml file
%.ml:
	$(call msg, "EXTRACT", $(notdir $@))
	$(Q)$(FSTAR) $(notdir $(subst .checked.lax,,$<)) --codegen $(CODEGEN) --extract_module $(basename $(notdir $(subst .checked.lax,,$<)))
	$(Q)@touch -c $@  ## SHOULD NOT BE NEEDED

# --------------------------------------------------------------------
# Dependency analysis for bootstrapping
# --------------------------------------------------------------------

# The dependence analysis starts from the main file and the unit-tests
# file as the roots, mentioning the the modules that are to be
# extracted. This emits dependences for each of the ML files we want
# to produce.

ROOTS :=
ROOTS += fstar/FStarC.Main.fst

# List here the files that define plugins in the library,
# so we make sure to also extract them and link them into F*.
PLUGIN_ROOTS += ../ulib/FStar.Order.fst
PLUGIN_ROOTS += ../ulib/FStarC.Reflection.TermEq.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Reflection.TermEq.Simple.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Reflection.V2.Compare.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Reflection.V2.Formula.fst
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.BV.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.CanonCommMonoidSimple.Equiv.fst
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.Canon.fst
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.Canon.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.CheckLN.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.MApply.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.MkProjectors.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.NamedView.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.Names.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.Parametricity.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.Print.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.SMT.fsti
PLUGIN_ROOTS += ../ulib/FStar.Tactics.Typeclasses.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.TypeRepr.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.V1.Logic.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.V2.Logic.fsti
PLUGIN_ROOTS += ../ulib/FStarC.Tactics.V2.SyntaxHelpers.fst

# We need the ML files for this
# ROOTS += ../ulib/FStar.Int128.fst
# ROOTS += ../ulib/FStar.UInt128.fst

$(ROOT_OUT)/.fstar_depend:
	$(call msg, "DEPEND")
	$(Q)$(FSTAR) --ext dep_pretag=FSTAR_ --dep full $(ROOTS) $(EXTRACT) --output_deps_to $@
	$(Q)mkdir -p $(CACHE_DIR)

depend: $(ROOT_OUT)/.fstar_depend

include $(ROOT_OUT)/.fstar_depend

all-ml: $(ALL_ML_FILES)
