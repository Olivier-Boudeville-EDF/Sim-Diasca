# Here are gathered explicit (immediate, static) rules for Sim-Diasca.
#
# See GNUmakerules-explicit.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: SIM_DIASCA_TOP must be set.


.PHONY: .PHONY-local


.PHONY-local: default-sim-diasca-rule make-config-links                \
			  make-config-links-recurse clean clean-probe clean-test   \
			  clean-test-logs clean-test-images clean-source-archives  \
			  clean-results clean-outputs



# The first non-generic, therefore default, rule:
default-sim-diasca-rule: all


# Ensures that, as soon a directory contains a test, there is an host
# configuration file, creating a symbolic link to the centralised version
# thereof.
#
# Run the clean-host-candidate-files target first, if wanting to rely on
# symbolic links pointing to the same centralised file.
#
make-config-links:
	@if [ ! -e "$(SIM_DIASCA_HOST_FILE)" ] ; then \
	if /bin/ls *_test.erl 1>/dev/null 2>&1 ; then \
	echo "  Adding link to centralised host file in $$(pwd)" ; \
	/bin/ln -sf $(SIM_DIASCA_TOP)/conf/$(SIM_DIASCA_HOST_FILE) ; fi ; fi


make-config-links-recurse:
	@for m in $(MODULES_DIRS) ; do \
	( cd $$m && $(MAKE) -s make-config-links make-config-links-recurse ) ; \
	done



clean: clean-probe clean-test

clean-probe:
	-@if [ $$(basename $$(pwd)) != "doc" ]; then /bin/rm -f *_Probe*.png *_probe*.png *_probe*.p *_probe*.dat *_Probe*.png *_Probe*.p *_Probe*.dat *-generated.png; fi


clean-test: clean-test-logs clean-test-images clean-source-archives


clean-test-logs:
	-@/bin/rm -f *_test.log


# *test*.png was removing images that were to be kept:
clean-test-images:
	-@if [ $$(basename $$(pwd)) != "doc" ]; then \
	/bin/rm -f *Test*.png; fi


# Code archive files may be created by tests:
clean-source-archives:
	-@/bin/rm -f *.sdar



# This target is quite slow to evaluate and a bit dangerous (actually it is not
# called by default, see the 'clean-all-results' target in the root
# GNUmakefile):
#
clean-results:
	@echo "    Cleaning all results for user $$USER in $$(pwd)"
	@for d in *-on-*-at-*-by-$$USER-* ; do /bin/rm -rf $$d ; done


clean-outputs: clean-results clean-traces
	@for m in $(MODULES_DIRS) ; do \
	( cd $$m && $(MAKE) -s clean-outputs ) ; \
	done


real-clean: clean-outputs


# Bootstrap section.


# Bootstrapped modules must be special-cased, as they should be built prior to
# any parse-transform using them (here, the Sim-Diasca one), and the resulting
# BEAM must be produced in the right directory (not at the engine root).
#
# As most paths (ex: the one leading to the Myriad parse transform) are
# relative, the corresponding directory change will make them irrelevant, so we
# add the correct ones (simpler than modifying the base paths).
#
# For actor_info, which is in src/core/generic (hence 3 levels away), and relies
#  only on myriad:
#
GENERIC_OFFSET := ../../..


# Both for usual build and OTP-based one:
BOOTSTRAP_OFFSET_PATHS_OPT_BEAMS := $(foreach dir,$(MYRIAD_BEAM_DIRS),-pz $(GENERIC_OFFSET)/$(dir)) -pz $(GENERIC_OFFSET)/../myriad/ebin


# Will not work (even if introducing a temporary variable such as 'ACTUAL_INC :=
# $(INC)'), as we are not at the bottom of all layers, for example Myriad's
# variables are still to be defined):
#
#BOOTSTRAP_OFFSET_PATHS_OPT_INCS := $(foreach dir,$(INC),-I$(GENERIC_OFFSET)/$(dir)) -I$(GENERIC_OFFSET)/../myriad/include


# So, instead of INC, we just hardcode what is needed:

BOOTSTRAP_INC_DIRS := include/core/generic

# Both for usual build and OTP-based one:
BOOTSTRAP_OFFSET_PATHS_OPT_INCS := $(foreach dir,$(BOOTSTRAP_INC_DIRS),-I$(GENERIC_OFFSET)/$(dir)) -I$(GENERIC_OFFSET)/../myriad/include


# See also: in GNUmakevars.inc, the BOOTSTRAP_MODULES variable.


# We still need a special build target here, so that the BEAM is generated in
# $(GENERIC_SRC) rather than at the root of Sim-Diasca:
#
# (refer to the Myriad counterpart bootstrap for more information)
#
$(GENERIC_SRC)/actor_info.beam: $(GENERIC_SRC)/actor_info.erl
	@echo "     Compiling bootstrap module $<"
	@#beam_file=$$(basename $@) ; cd $$(dirname $@) && pwd && echo $(ERLANG_COMPILER) $(BOOTSTRAP_OFFSET_PATHS_OPT_BEAMS) $(BOOTSTRAP_OFFSET_PATHS_OPT_INCS) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )
	@beam_file=$$(basename $@) ; cd $$(dirname $@) && $(ERLANG_COMPILER) $(BOOTSTRAP_OFFSET_PATHS_OPT_BEAMS) $(BOOTSTRAP_OFFSET_PATHS_OPT_INCS) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $$beam_file $$( echo $$beam_file | sed 's|.beam$$|.erl|1' )
