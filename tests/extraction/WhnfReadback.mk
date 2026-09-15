all: $(OUTPUT_DIR)/WhnfReadback.test

# Compare default WHNF readback with explicit opt-in and opt-out, including
# polymorphic, nested, stuck and over-applied projectors.
$(OUTPUT_DIR)/WhnfReadback.test: $(CACHE_DIR)/WhnfReadback.fst.checked WhnfReadback.expected.ml Makefile WhnfReadback.mk $(FSTAR_EXE)
	@mkdir -p $(OUTPUT_DIR)/whnf-default $(OUTPUT_DIR)/whnf-eager $(OUTPUT_DIR)/whnf-deferred
	@$(FSTAR) $< --codegen OCaml --extract_module WhnfReadback --odir $(OUTPUT_DIR)/whnf-default
	@$(FSTAR) $< --codegen OCaml --extract_module WhnfReadback --ext defer_whnf_universe_erasure=0 --odir $(OUTPUT_DIR)/whnf-eager
	@$(FSTAR) $< --codegen OCaml --extract_module WhnfReadback --ext defer_whnf_universe_erasure=1 --odir $(OUTPUT_DIR)/whnf-deferred
	@sed 's/ *$$//' $(OUTPUT_DIR)/whnf-default/WhnfReadback.ml > $(OUTPUT_DIR)/whnf-default/WhnfReadback.normalized.ml
	@diff -u WhnfReadback.expected.ml $(OUTPUT_DIR)/whnf-default/WhnfReadback.normalized.ml
	@diff -u $(OUTPUT_DIR)/whnf-default/WhnfReadback.ml $(OUTPUT_DIR)/whnf-eager/WhnfReadback.ml
	@diff -u $(OUTPUT_DIR)/whnf-default/WhnfReadback.ml $(OUTPUT_DIR)/whnf-deferred/WhnfReadback.ml
	@touch $@
