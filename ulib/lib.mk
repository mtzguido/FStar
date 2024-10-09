include $(FSTAR_HOME)/.common.mk
export FSTAR_HOME # because of the recursive calls to `make`

.PHONY: all
all: ocaml

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

FSTAR := $(FSTAR_EXE) $(SIL) $(FSTAR_OPTIONS)

EXTRACT :=
EXTRACT += --extract '*'
EXTRACT += --extract '-LowStar.Printf'

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

$(CACHE_DIR)/.fstar_depend:
	$(call msg, "DEPEND")
	$(Q)$(FSTAR) --dep full $(ROOTS) $(EXTRACT) --output_deps_to $@
	$(Q)mkdir -p $(CACHE_DIR)

depend: $(CACHE_DIR)/.fstar_depend
include $(CACHE_DIR)/.fstar_depend

all-ml: $(ALL_ML_FILES)
all-checked: $(ALL_CHECKED_FILES)
