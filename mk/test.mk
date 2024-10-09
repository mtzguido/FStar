HINTS_ENABLED?=--use_hints
WARN_ERROR=
OTHERFLAGS+=$(WARN_ERROR)

$(call need, FSTAR_EXE, fstar.exe to be used)

# Set ADMIT=1 to admit queries
ADMIT ?=
MAYBE_ADMIT = $(if $(ADMIT),--admit_smt_queries true)

FSTAR = $(FSTAR_EXE) $(OTHERFLAGS) $(MAYBE_ADMIT) $(HINTS_ENABLED) $(WITH_CACHE_DIR)
