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

Q?=@
SIL?=--silent
RESMON=
ifneq ($(V),)
	Q=
	SIL=
endif

define NO_RAMON_ERR
ramon not found:
  To use RESOURCEMONITOR=1, the `ramon` tool must be installed and in your $$PATH.
  You can get it from: [https://github.com/mtzguido/ramon]
endef

define msg =
@printf "  %-8s  %s\n" $(1) $(2)
endef

define resmon_to =
ramon -o $(1).$(MONPREFIX)resinfo
endef

# Passing RESOURCEMONITOR=1 will create .resinfo files through the source tree with
# information about the time and space taken by each F* invocation.
ifneq ($(RESOURCEMONITOR),)
	ifeq ($(shell which ramon),)
		_ := $(error $(NO_RAMON_ERR)))
	endif
	ifneq ($(MONID),)
		MONPREFIX=$(MONID).
	endif
	RESMON=$(call resmon_to,$@)
else
	# Undefine resmon_to
define resmon_to =
endef
endif

# Can be called as $(call maybe_cygwin_path,...)
#   where ... is the argument

maybe_cygwin_path=$(if $(findstring $(OS),Windows_NT),$(shell cygpath -m $(1)),$(1))

# Ensure that any failing rule will not create its target file.
# In other words, make `make` less insane.
.DELETE_ON_ERROR:
