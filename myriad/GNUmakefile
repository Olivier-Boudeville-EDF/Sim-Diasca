MYRIAD_TOP = .


.PHONY: help help-intro help-myriad help-hints help-batch                     \
		register-version-in-header register-myriad list-beam-dirs             \
		add-prerequisite-plts prepare-base-plt add-erlhdf5-plt add-jsx-plt    \
		add-sqlite3-plt link-plt clean-ast-outputs clean-local stats          \
		info-paths info-settings info-compile info-parse-transform info-sync  \
		info-check info-conditionals info-rebar3


#MODULES_DIRS = contrib src doc conf
MODULES_DIRS = src doc conf test


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE := true


# Default target:
help: help-intro help-myriad help-hints help-batch


include $(MYRIAD_TOP)/GNUmakesettings.inc


# The base PLT is not the predecessor one (i.e. the Erlang PLT), as
# prerequisites may have to be taken into account, and have already started
# feeding PLT_FILE:
#
BASE_PLT := $(PLT_FILE)



help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-myriad:
	@echo "  - 'all':        builds everything (recursively, from current directory)"
	@echo "  - 'clean':      cleans compiled code (recursively, from current directory)"
	@echo "  - 'real-clean': cleans everything (from the root of any package)"
	@echo "  - 'X_beam':     generates module X.beam from source X.erl (and possibly header X.hrl)"
	@echo "  - 'X_run':      runs test case X_test.beam"
	@echo "  - 'X_exec':     runs application X_app.beam"
	@echo "  - 'doc':        generates documentation"
	@echo "  - 'info':       displays make-related key variables"
	@echo "  - 'help':       displays this help"


help-hints: help-batch


help-batch:
	@echo " By default, code is run in interactive mode."
	@echo " To run in batch mode, add: CMD_LINE_OPT=\"--batch\" to the command line."
	@echo " For example, one may run: 'make X_run CMD_LINE_OPT=\"--batch\"'"
	@echo " Thus it may be convenient to define, in one's shell, the BATCH"
	@echo " variable as 'CMD_LINE_OPT=\"--batch\"', so that one can then simply run:"
	@echo " 'make X_run \$$BATCH' (of course, to save more typing, one may define "
	@echo " additional make targets)"


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 50 ; else \
	$(MAKE) register-myriad ; fi


register-myriad:
	@echo "-define( myriad_version, \"$(MYRIAD_VERSION)\" )." >> $(VERSION_FILE)



# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(MYRIAD_BEAM_DIRS) ; do echo $$(readlink -f $$d) ; done



add-prerequisite-plts: prepare-base-plt \
					   add-erlhdf5-plt add-jsx-plt add-sqlite3-plt link-plt


# So that in all cases we start by the same PLT name:
prepare-base-plt:
	@echo "Copying predecessor PLT $(PREDECESSOR_PLT) to $(PLT_FILE)"
	@/bin/cp -f $(PREDECESSOR_PLT) $(PLT_FILE)


# First prerequisite operates on predecessor (here, the Erlang PLT):
add-erlhdf5-plt:
	@if [ "$(USE_HDF5)" == "true" ] ; then echo "   Generating PLT for prerequisite erlhdf5" ; $(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(ERLHDF5_BASE)/ebin --plt $(PLT_FILE); if [ $$? -eq 1 ] ; then exit 1 ; fi ; else echo "(no PLT determined for non-available erlhdf5 prerequisite; unknown functions in the erlhdf5 module will be found)" ; fi


# From the second, operating on the current PLT:
add-jsx-plt:
	@if [ "$(USE_JSON)" == "true" ] ; then echo "   Generating PLT for prerequisite jsx" ; $(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(JSX_BASE)/ebin --plt $(PLT_FILE); if [ $$? -eq 1 ] ; then exit 1 ; fi ; else echo "(no PLT determined for non-available jsx prerequisite; unknown functions in the jsx module will be found)" ; fi


add-sqlite3-plt:
	@if [ "$(USE_SQLITE)" == "true" ] ; then echo "   Generating PLT for prerequisite sqlite3" ; $(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(SQLITE3_BASE)/ebin --plt $(PLT_FILE); if [ $$? -eq 1 ] ; then exit 1 ; fi ; else echo "(no PLT determined for non-available sqlite3 prerequisite; unknown functions in the sqlite3 module will be found)" ; fi


# As upper layers may rely on the 'myriad' naming:
link-plt:
	@if [ ! "$(PLT_FILE)" = "$(MYRIAD_PLT_FILE)" ]; then /bin/ln -s --force $(PLT_FILE) $(MYRIAD_PLT_FILE) ; fi


# Removes the text files that may be spit by the myriad parse transform for
# debugging purposes:
#
clean-ast-outputs:
	@echo "  Removing AST output text files"
	-@find . -name 'Output-AST-for-module-*.txt' -exec /bin/rm -f '{}' ';'


clean: clean-local

clean-local:
	-@/bin/rm -rf .rebar3


stats:
	@$(MAKE_CODE_STATS) $(MYRIAD_TOP)


info-paths:
	@echo "BEAM_PATH_OPT = $(BEAM_PATH_OPT)"


info-settings:
	@echo "USE_HDF5   = $(USE_HDF5)"
	@echo "USE_JSON   = $(USE_JSON)"
	@echo "USE_JSX    = $(USE_JSX)"
	@echo "USE_JIFFY  = $(USE_JIFFY)"
	@echo "USE_REST   = $(USE_REST)"
	@echo "USE_SQLITE = $(USE_SQLITE)"


info-compile:
	@echo "ERLANG_COMPILER_BASE_OPT = $(ERLANG_COMPILER_BASE_OPT)"
	@echo "ABS_BEAM_DIRS = $(ABS_BEAM_DIRS)"
	@echo "BEAM_DIRS = $(BEAM_DIRS)"
	@echo "BEAM_PATH_OPT = $(BEAM_PATH_OPT)"
	@echo "INC = $(INC)"
	@echo "ERLANG_COMPILER_EXEC_TARGET_OPT = $(ERLANG_COMPILER_EXEC_TARGET_OPT)"
	@echo "ERLANG_COMPILER_DEBUG_OPT = $(ERLANG_COMPILER_DEBUG_OPT)"
	@echo "ERLANG_COMPILER_NATIVE_COMPILATION_OPT = $(ERLANG_COMPILER_NATIVE_COMPILATION_OPT)"
	@echo "ERLANG_COMPILER_WARNING_OPT = $(ERLANG_COMPILER_WARNING_OPT)"
	@echo "ERLANG_COMPILER_OPT_BASE = $(ERLANG_COMPILER_OPT_BASE)"
	@echo "OVERALL_PZ_OPT = $(OVERALL_PZ_OPT)"
	@echo "ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES = $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES)"
	@echo "ERLANG_INTERPRETER = $(ERLANG_INTERPRETER)"
	@echo "UNSUPPORTED_SOURCES = $(UNSUPPORTED_SOURCES)"
	@echo "NOTIFIER_TOOL = $(NOTIFIER_TOOL)"


info-parse-transform:
	@echo "BOOTSTRAP_MODULES = $(BOOTSTRAP_MODULES)"
	@echo "ERLANG_COMPILER_OPT_FOR_PT = $(ERLANG_COMPILER_OPT_FOR_PT)"
	@echo "META_BEAM_FILES = $(META_BEAM_FILES)"
	@echo "ERLANG_COMPILER_PARSE_TRANSFORM_OPT = $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT)"


info-conditionals:
	@echo "MYRIAD_DEBUG_FLAGS = $(MYRIAD_DEBUG_FLAGS)"
	@echo "MYRIAD_CHECK_FLAGS = $(MYRIAD_CHECK_FLAGS)"


info-sync:
	@echo "SYNC_TOOL = $(SYNC_TOOL)"
	@echo "SSH_PORT = $(SSH_PORT)"
	@echo "SSH_OPT = $(SSH_OPT)"
	@echo "SYNC_OPT = $(SYNC_OPT)"
	@echo "WEB_SRV = $(WEB_SRV)"
	@echo "CEYLAN_SYNC_TARGET_ROOT = $(CEYLAN_SYNC_TARGET_ROOT)"


info-check:
	@echo "DIALYZER = $(DIALYZER)"
	@echo "DIALYZER_OPT = $(DIALYZER_OPT)"


# Useful to extract information to be specified in a parallel rebar.config:
info-rebar3:
	@echo "BOOTSTRAP_MODULES = $(BOOTSTRAP_MODULES)"
	@echo "INC = $(INC)"
	@echo "MYRIAD_REBAR_INCS = $(MYRIAD_REBAR_INCS)"
	@echo "MYRIAD_REBAR_FIND_SRC_EXCLUDES = $(MYRIAD_REBAR_FIND_SRC_EXCLUDES)"
	@echo "MYRIAD_REBAR_FIND_SRC_OPT = $(MYRIAD_REBAR_FIND_SRC_OPT)"
