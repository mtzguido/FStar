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
# FSTAR_EXE		?= $(FSTAR_HOME)/stage1/bare/bin/fstar.exe
# CACHE_DIR 		?= $(CACHE_DIR)/plugins.checked
# OUTPUT_DIR	  ?= $(CACHE_DIR)/plugins.ml
# CODEGEN       ?= Plugin
# SRC           ?= .

FSTAR_OPTIONS += $(OTHERFLAGS)
FSTAR_OPTIONS += --lax
# FSTAR_OPTIONS += --warn_error -271-241-319-274
FSTAR_OPTIONS += --cache_dir "$(CACHE_DIR)"
FSTAR_OPTIONS += --cache_checked_modules
FSTAR_OPTIONS += --odir "$(OUTPUT_DIR)"
FSTAR_OPTIONS += --no_default_includes
FSTAR_OPTIONS += --include $(SRC)

# FIXME: This should be somehow fixed better. We should distinguish the
# namespaces of the 'old' fstarlib and the new one.
# FSTAR_OPTIONS += --no_default_includes
# FSTAR_OPTIONS += --include $(realpath $(FSTAR_HOME)/ulib/)

FSTAR = $(FSTAR_EXE) $(SIL) $(FSTAR_OPTIONS)

# FIXME: Maintaining this list sucks. Could **the module** itself specify whether it is
# noextract?
EXTRACT :=
EXTRACT += --extract '*'
EXTRACT += --extract -Prims
EXTRACT += --extract -FStar.Pervasives.Native
EXTRACT += --extract -FStar.All
EXTRACT += --extract -FStar.Ghost
EXTRACT += --extract -FStar.Heap
EXTRACT += --extract -FStar.Bytes
EXTRACT += --extract -FStar.Char
EXTRACT += --extract -FStar.Exn
EXTRACT += --extract -FStar.Float
EXTRACT += --extract -FStar.Int16
EXTRACT += --extract -FStar.Int32
EXTRACT += --extract -FStar.Int64
EXTRACT += --extract -FStar.Int8
EXTRACT += --extract +FStar.Int.Cast.Full
EXTRACT += --extract -FStar.List
EXTRACT += --extract +FStar.List.Pure.Base
EXTRACT += --extract +FStar.List.Tot.Properties
EXTRACT += --extract -FStar.Monotonic.Heap
EXTRACT += --extract -FStar.HyperStack.ST
EXTRACT += --extract -FStar.Option
EXTRACT += --extract -FStar.Printf
EXTRACT += --extract -FStar.Range
EXTRACT += --extract -FStar.ST
EXTRACT += --extract -FStar.String
EXTRACT += --extract -FStar.TSet
EXTRACT += --extract -FStar.UInt16
EXTRACT += --extract -FStar.UInt32
EXTRACT += --extract -FStar.UInt64
EXTRACT += --extract -FStar.UInt8
EXTRACT += --extract -FStar.Util
EXTRACT += --extract -FStar.Version
  
# EXTRACT += --extract -FStar.BitVector
# EXTRACT += --extract -FStar.Calc

# We first lax type-check each file, producing a .checked.lax file
# We touch the file, because if F* determined that the .checked.lax
# file was already up to date, it doesn't touch it. Touching it here
# ensures that if this rule is successful then %.checked.lax is more
# recent than its dependences.
%.checked.lax:
	$(call msg, "LAXCHECK", $(basename $(basename $(notdir $@))))
	$(FSTAR) $(if $(findstring /ulib/,$<),,--MLish) $<
	@touch -c $@  ## SHOULD NOT BE NEEDED

# And then, in a separate invocation, from each .checked.lax we
# extract an .ml file
%.ml:
	$(call msg, "EXTRACT", $(notdir $@))
	$(FSTAR) $(notdir $(subst .checked.lax,,$<)) --codegen $(CODEGEN) --extract_module $(basename $(notdir $(subst .checked.lax,,$<)))
	@touch -c $@  ## SHOULD NOT BE NEEDED

# --------------------------------------------------------------------
# Dependency analysis for bootstrapping
# --------------------------------------------------------------------

# List here the files that define plugins in the library,
# so we make sure to also extract them and link them into F*.
# MUST BE NON EMPTY OR WE WILL EXTRACT THE ENTIRE LIBRARY
ROOTS += ../ulib/FStar.Tactics.Effect.fsti
ROOTS += ../ulib/FStar.Order.fst
ROOTS += ../ulib/FStar.Reflection.TermEq.fsti
ROOTS += ../ulib/FStar.Reflection.TermEq.Simple.fsti
ROOTS += ../ulib/FStar.Reflection.V2.Compare.fsti
ROOTS += ../ulib/FStar.Reflection.V2.Formula.fst
ROOTS += ../ulib/FStar.Tactics.BV.fsti
ROOTS += ../ulib/FStar.Tactics.CanonCommMonoidSimple.Equiv.fst
ROOTS += ../ulib/FStar.Tactics.Canon.fst
ROOTS += ../ulib/FStar.Tactics.Canon.fsti
ROOTS += ../ulib/FStar.Tactics.CheckLN.fsti
ROOTS += ../ulib/FStar.Tactics.MApply0.fsti
ROOTS += ../ulib/FStar.Tactics.MkProjectors.fsti
ROOTS += ../ulib/FStar.Tactics.NamedView.fsti
ROOTS += ../ulib/FStar.Tactics.Names.fsti
ROOTS += ../ulib/FStar.Tactics.Parametricity.fsti
ROOTS += ../ulib/FStar.Tactics.Print.fsti
ROOTS += ../ulib/FStar.Tactics.SMT.fsti
ROOTS += ../ulib/FStar.Tactics.Typeclasses.fsti
ROOTS += ../ulib/FStar.Tactics.TypeRepr.fsti
ROOTS += ../ulib/FStar.Tactics.V1.Logic.fsti
ROOTS += ../ulib/FStar.Tactics.V2.Logic.fsti
ROOTS += ../ulib/FStar.Tactics.V2.SyntaxHelpers.fst
ROOTS += ../ulib/FStar.Tactics.Visit.fst

$(CACHE_DIR)/.depend:
	$(call msg, "DEPEND")
	$(FSTAR) --dep full $(ROOTS) $(EXTRACT) --output_deps_to $@
	mkdir -p $(CACHE_DIR)

depend: $(CACHE_DIR)/.depend
include $(CACHE_DIR)/.depend

all-ml: $(ALL_ML_FILES)
