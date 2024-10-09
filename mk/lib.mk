include $(FSTAR_HOME)/.common.mk

.PHONY: all
all: verify ocaml

.PHONY: ocaml
ocaml: all-ml

.PHONY: verify
verify: all-checked

FSTAR_OPTIONS += $(OTHERFLAGS)
FSTAR_OPTIONS += --use_hints
FSTAR_OPTIONS += --hint_dir $(SRC)/.hints
FSTAR_OPTIONS += --cache_dir "$(CACHE_DIR)"
FSTAR_OPTIONS += --cache_checked_modules
FSTAR_OPTIONS += --odir "$(OUTPUT_DIR)"
FSTAR_OPTIONS += --no_default_includes
FSTAR_OPTIONS += --include $(SRC)

FSTAR_REALIZED_MODULES=All Buffer Bytes Char CommonST Constructive Dyn Float Ghost Heap Monotonic.Heap \
	HyperStack.All HyperStack.ST HyperStack.IO Int16 Int32 Int64 Int8 IO \
	List List.Tot.Base Mul Option Pervasives.Native ST Exn String \
	UInt16 UInt32 UInt64 UInt8 \
	Pointer.Derived1 Pointer.Derived2 \
	Pointer.Derived3 \
	BufferNG \
	TaggedUnion \
	Bytes Util \
	Tactics \
	Reflection \
	InteractiveHelpers \
	Class \
	Range \
	Vector.Base Vector.Properties Vector TSet
	# prims is realized by default hence not included in this list

NOEXTRACT_STEEL_MODULES = -FStar.MSTTotal -FStar.MST -FStar.NMSTTotal -FStar.NMST

NOEXTRACT_MODULES:=$(addprefix -FStar., $(FSTAR_REALIZED_MODULES) Printf) \
  -FStar.ModifiesGen \
  -LowStar.Printf +FStar.List.Pure.Base +FStar.List.Tot.Properties +FStar.Int.Cast.Full $(NOEXTRACT_STEEL_MODULES)


FSTAR := $(FSTAR_EXE) $(SIL) $(FSTAR_OPTIONS)

EXTRACT :=
EXTRACT += --extract '* $(NOEXTRACT_MODULES)'

# We first lax type-check each file, producing a .checked.lax file
# We touch the file, because if F* determined that the .checked.lax
# file was already up to date, it doesn't touch it. Touching it here
# ensures that if this rule is successful then %.checked.lax is more
# recent than its dependences.
%.checked:
	$(call msg, "CHECK", $(basename $(basename $(notdir $@))))
	$(Q)$(FSTAR) $<
	@# HACK: finding FStarC modules
	$(Q)@touch -c $@  ## SHOULD NOT BE NEEDED

# And then, in a separate invocation, from each .checked.lax we
# extract an .ml file
%.ml:
	$(call msg, "EXTRACT", $(notdir $@))
	@# HACK we use notdir to get the module name since we need to pass in the
	@# fst (not the checked file), but we don't know where it is, so this is
	@# relying on F* looking in its include path. sigh.
	$(Q)$(FSTAR) $(notdir $(subst .checked,,$<)) --codegen $(CODEGEN) --extract_module $(basename $(notdir $(subst .checked,,$<)))
	$(Q)@touch -c $@  ## SHOULD NOT BE NEEDED

ROOTS :=
ROOTS += $(shell find $(SRC) -name *.fst)
ROOTS += $(shell find $(SRC) -name *.fsti)

# Filter out legacy/
FILTER_OUT = $(foreach v,$(2),$(if $(findstring $(1),$(v)),,$(v)))
ROOTS := $(call FILTER_OUT,legacy/,$(ROOTS))

$(CACHE_DIR)/.fstar_depend:
	$(call msg, "DEPEND")
	$(Q)$(FSTAR) --dep full $(ROOTS) $(EXTRACT) --output_deps_to $@
	$(Q)mkdir -p $(CACHE_DIR)

depend: $(CACHE_DIR)/.fstar_depend
include $(CACHE_DIR)/.fstar_depend

all-ml: $(ALL_ML_FILES)
all-checked: $(ALL_CHECKED_FILES)
