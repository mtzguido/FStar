FSTAR_ROOT ?= ../..
include $(FSTAR_ROOT)/mk/test.mk

all: verify-all $(OUTPUT_DIR)/test.exe

EXTRACT = RBTreeIntrinsic

OCAML_DEFAULT_FLAGS += -I $(OUTPUT_DIR) -w -31

$(OUTPUT_DIR)/test.exe: $(OUTPUT_DIR)/$(EXTRACT).ml | $(OUTPUT_DIR)
	@echo 'let _ = test()' >> $^
	$(OCAMLOPT) -package fstar_lib -linkpkg -o $(OUTPUT_DIR)/test.exe $^
