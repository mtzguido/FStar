# This makefile is included from several other makefiles in the tree.

# It enables configurably 'silent' rules, that do not
# print output unless V=1 is set. When writing a rule, you can do as
# follows (taken from src/Makefile.boot):
#
# ocaml-output/%.ml:
# 	$(call msg, "EXTRACT", $(notdir $@))
# 	$(Q)$(BENCHMARK_PRE) $(FSTAR_C) $(SIL) $(notdir $(subst .checked.lax,,$<)) \
#                   --codegen OCaml \
#                   --extract_module $(basename $(notdir $(subst .checked.lax,,$<)))
#
# This unconditionally prints a message like '[EXTRACT FStar_Syntax_Subst.ml]'
# (`notdir` is used to omit the directory of the target) and then
# proceeds to execute the F* invocation silently (since $(Q) expands to
# "@"). However, calling the same rule with `make V=1` will still print
# the message and then print the F* invocation before executing.
#
# Besides that, when not using V=1, F* receives the --silent flag to
# reduce non-critical output.

# It also defines some other utilities for resource monitoring and
# paths manipulation for cygwin

MAKEFLAGS += --no-builtin-rules
Q?=@
SIL?=--silent
RUNLIM=
ifneq ($(V),)
	Q=
	SIL=
else
	MAKEFLAGS += -s
endif

define NO_RUNLIM_ERR
runlim not found:
  To use RESOURCEMONITOR=1, the `runlim` tool must be installed and in your $$PATH.
  It must also be a recent version supporting the `-p` option.
  You can get it from: [https://github.com/arminbiere/runlim]
endef

define msg =
@printf "  %-14s  %s\n" $(1) $(2)
endef

# Check that we are under FSTAR_HOME. Otherwise abort now.
# REL_CURDIR=$(shell realpath --relative-base=$(FSTAR_HOME) $(CURDIR))
# REL_CURDIR_0=$(shell echo $(REL_CURDIR) | head -c 1)
# ifeq ($(REL_CURDIR_0),/)
#   $(error "FSTAR_HOME points outside of this repo. ($(FSTAR_HOME))")
# endif
# Hmmmmm, no longer useful with sub-installs.

# Passing RESOURCEMONITOR=1 will create .runlim files through the source tree with
# information about the time and space taken by each F* invocation.
ifneq ($(RESOURCEMONITOR),)
	ifeq ($(shell which runlim),)
		_ := $(error $(NO_RUNLIM_ERR)))
	endif
	ifneq ($(MONID),)
		MONPREFIX=$(MONID).
	endif
	RUNLIM=runlim -p -o $@.$(MONPREFIX)runlim
endif

# Can be called as $(call maybe_cygwin_path,...)
#   where ... is the argument

maybe_cygwin_path=$(if $(findstring $(OS),Windows_NT),$(shell cygpath -m $(1)),$(1))

# Ensure that any failing rule will not create its target file.
# In other words, make `make` less insane.
.DELETE_ON_ERROR:

.DEFAULT_GOAL:=undef
.PHONY: undef
undef:
	$(error "This makefile does not have a default goal")
