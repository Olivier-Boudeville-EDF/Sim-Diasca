SIM_DIASCA_TOP = .


.PHONY: help help-sim-diasca                                          \
		all register-version-in-header register-sim-diasca            \
		list-beam-dirs list-beam-dirs add-prerequisite-plts link-plt  \
		clean clean-local clean-host-candidate-files stats            \
		info-local info-tools info-network info-versions              \
		info-context info-versions info-sim-diasca info-compile       \
		info-conditionals info-deps


MODULES_DIRS = src test doc conf


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


# Default target:
help: help-intro help-sim-diasca


# Not relevant enough:@cd $(TRACES_TOP) && $(MAKE) -s help-traces
help-sim-diasca:
	@echo "  - 'all':           builds everything (recursively, from current directory)"
	@echo "  - 'clean':         cleans compiled code (recursively, from current directory)"
	@echo "  - 'clean-outputs': cleans all runtime outputs, like traces or results (recursively, from current directory)"
	@echo "  - 'real-clean':    cleans everything (from the root of any layer)"
	@echo "  - 'X_beam':        generates module X.beam from source X.erl (and possibly header X.hrl)"
	@echo "  - 'X_run':         runs simulation case X_case.beam or test case X_test.beam"
	@echo "  - 'X_exec':        runs application X_app.beam"
	@echo "  - 'info':          displays make-related key variables"
	@echo "  - 'help':          displays this help"
	@cd $(MYRIAD_TOP) && $(MAKE) -s help-batch



register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ]; then \
	echo "  Error, no version file defined." 1>&2; exit 53; else \
	$(MAKE) -s register-sim-diasca; fi


register-sim-diasca:
	@echo "-define( sim_diasca_version, \"$(SIM_DIASCA_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(SIM_DIASCA_BEAM_DIRS); do echo $$(readlink -f $$d); done


add-prerequisite-plts: link-plt


# As upper layers may rely on the 'sim-diasca' naming:
link-plt:
	@if [ ! "$(PLT_FILE)" = "$(SIM_DIASCA_PLT_FILE)" ]; then /bin/ln -s --force $(PLT_FILE) $(SIM_DIASCA_PLT_FILE); fi


stats:
	@$(MAKE_CODE_STATS) $(SIM_DIASCA_TOP)


info-local:
	@echo "BEAM_PATHS = $(BEAM_PATHS)"
	@echo "ARCHIVE_LOCATION = $(ARCHIVE_LOCATION)"


info-tools:
	@echo "ERLANG_INTERPRETER = $(ERLANG_INTERPRETER)"
	@echo "ERLANG_COMPILER = $(ERLANG_COMPILER)"
	@echo "LOGMX = $(LOGMX)"


info-network:
	@echo "FQDN = $(FQDN)"



clean: clean-local


# Was:
#  @find . \( -name '*.dia~' -o -name 'svn-commit.tmp' \) \
#	-exec /bin/rm -f '{}' ';'
#
clean-local:
	@find . -name '*.dia~' -exec /bin/rm -f '{}' ';'



# When moving source archives here and there, often symbolic links pointing to
# sim-diasca-host-candidates.txt files are replaced by actual copies.
#
# We get rid of them here, see the make-config-links-recurse rule to repopulate
# these files with links.
#
# We do not want to recurse in 'conf' as the original lies there:
#
clean-host-candidate-files:
	@find src -name "$(SIM_DIASCA_HOST_FILE)" -exec /bin/rm -f '{}' ';'



# Typically useful to know the software context for continuous integration:
info-context: info-platform info-versions


info-versions:
	@echo "MYRIAD_VERSION = $(MYRIAD_VERSION)"
	@echo "WOOPER_VERSION = $(WOOPER_VERSION)"
	@echo "TRACES_VERSION = $(TRACES_VERSION)"
	@echo "SIM_DIASCA_VERSION = $(SIM_DIASCA_VERSION)"


info-sim-diasca:


info-compile: info-compile-traces


info-conditionals:
	@echo "SIM_DIASCA_DEBUG_FLAGS = $(SIM_DIASCA_DEBUG_FLAGS)"
	@echo "SIM_DIASCA_CHECK_FLAGS = $(SIM_DIASCA_CHECK_FLAGS)"


info-deps:
	@echo "MYRIAD_TOP = $(MYRIAD_TOP) (i.e. $$(realpath $(MYRIAD_TOP)))"
	@echo "WOOPER_TOP = $(WOOPER_TOP)) (i.e. $$(realpath $(WOOPER_TOP)))"
	@echo "TRACES_TOP = $(TRACES_TOP)) (i.e. $$(realpath $(TRACES_TOP)))"


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
