MYRIAD_TOP = .


.PHONY: help help-myriad help-hints help-batch                                \
		register-version-in-header register-myriad list-beam-dirs             \
		add-prerequisite-plts prepare-base-plt add-erlhdf5-plt add-jsx-plt    \
		add-sqlite3-plt link-plt clean-ast-outputs clean-local stats          \
		info-context info-versions info-paths                                 \
		info-settings info-json info-sql                                      \
		info-parse-transform-local info-conditionals info-glsl


#MODULES_DIRS = contrib src doc conf
MODULES_DIRS = src doc conf test


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE := true


# Default target:
help: help-intro help-myriad help-hints help-batch


include $(MYRIAD_TOP)/GNUmakesettings.inc


# The base PLT is not the predecessor one (i.e. the Erlang PLT), as
# prerequisites may have to be taken into account, and may have already started
# feeding PLT_FILE:
#
BASE_PLT := "$(PLT_FILE)"



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
	@if [ -z "$(VERSION_FILE)" ]; then \
	echo "Error, no version file defined." 1>&2; exit 50; else \
	$(MAKE) register-myriad; fi


register-myriad:
	@echo "-define( myriad_version, \"$(MYRIAD_VERSION)\" )." >> $(VERSION_FILE)



# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(MYRIAD_BEAM_DIRS); do echo $$(readlink -f $$d); done


list-beam-relative-paths:
	@cd src && echo $(MYRIAD_BEAM_DIRS)


add-prerequisite-plts: prepare-base-plt \
					   add-erlhdf5-plt add-jsx-plt add-sqlite3-plt link-plt


# So that in all cases we start by the same PLT name:
prepare-base-plt:
	@echo "Copying predecessor PLT $(PREDECESSOR_PLT_FILE) as $(PLT_FILE)"
	@/bin/cp -f "$(PREDECESSOR_PLT_FILE)" "$(PLT_FILE)"


# First prerequisite operates on predecessor (here, the Erlang PLT):
add-erlhdf5-plt:
	@if [ "$(USE_HDF5)" = "true" ]; then echo "   Generating PLT for prerequisite erlhdf5"; $(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(ERLHDF5_BASE)/ebin --plt $(PLT_FILE); if [ $$? -eq 1 ]; then exit 1; fi; else echo "  (no PLT determined for non-available erlhdf5 prerequisite; unknown functions in the erlhdf5 module will be found)" 1>&2; fi


# From the second, operating on the current PLT:
add-jsx-plt:
	@if [ "$(USE_JSON)" = "true" ]; then echo "   Generating PLT for prerequisite jsx"; $(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(JSX_BASE)/ebin --plt $(PLT_FILE); if [ $$? -eq 1 ]; then exit 1; fi; else echo "  (no PLT determined for non-available jsx prerequisite; unknown functions in the jsx module will be found)" 1>&2; fi


add-sqlite3-plt:
	@if [ "$(USE_SQLITE)" = "true" ]; then echo "   Generating PLT for prerequisite sqlite3"; $(DIALYZER) --add_to_plt --output_plt $(PLT_FILE) -r $(SQLITE3_BASE)/ebin --plt $(PLT_FILE); if [ $$? -eq 1 ]; then exit 1; fi; else echo "  (no PLT determined for non-available sqlite3 prerequisite; unknown functions in the sqlite3 module will be found)" 1>&2; fi


# As upper layers may rely on the 'myriad' naming:
link-plt:
	@if [ ! "$(PLT_FILE)" = "$(MYRIAD_PLT_FILE)" ]; then /bin/ln -s --force $(PLT_FILE) $(MYRIAD_PLT_FILE); fi


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



# Note that all info-* targets that are not specific to Myriad shall be in
# GNUmakerules-explicit.inc so that they can be inherited by deriving layers.

info-versions:
	@echo "MYRIAD_VERSION = $(MYRIAD_VERSION)"
	@echo "ERLANG_TARGET_VERSION = $(ERLANG_TARGET_VERSION)"


info-parse-transform: info-parse-transform-local

info-parse-transform-local:
	@echo "META_BEAM_FILES = $(META_BEAM_FILES)"

info-conditionals:
	@echo "MYRIAD_DEBUG_FLAGS = $(MYRIAD_DEBUG_FLAGS)"
	@echo "MYRIAD_CHECK_FLAGS = $(MYRIAD_CHECK_FLAGS)"


info-glsl:
	@echo "GLSL_REF_COMPILER = $(GLSL_REF_COMPILER)"
	@echo "GLSL_REF_COMPILER_OPT = $(GLSL_REF_COMPILER_OPT)"
