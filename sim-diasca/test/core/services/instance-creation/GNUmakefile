SIM_DIASCA_TOP = ../../../..


.PHONY: all test run-tests info-tests


all:


test: run-tests


# From the simplest to the most demanding, to help the troubleshooting:
#
# (moreover there are a few dependencies between tests, for example the one
# saving a simulation archive should be run before the one loading it)
#
ORDERED_TESTS = \
	initial_actor_programmatic_creations_case.erl                 \
	initial_actor_data_based_creations_case.erl                   \
	initial_actor_data_based_and_programmatic_creations_case.erl  \
	initial_actor_performing_nested_initial_creation_case.erl     \
	initial_actors_performing_nested_initial_creations_case.erl   \
	runtime_actor_simple_creation_case.erl                        \
	runtime_actor_complex_creation_case.erl


run-tests:
	@echo "  Running all tests now in batch mode:"
	@for t in $(ORDERED_TESTS); do \
	target=$$(echo $$t|sed 's|\_case.erl|_run|1') ; \
	echo " ---- Running $$target ----" ; \
	if ! $(MAKE) $$target CMD_LINE_OPT="--batch" ; then \
	echo "######## Test $$target failed! ########" 1>&2 ; exit 120 ; \
	else sleep 1 ; echo ; fi ; done


info-tests:
	@echo "ORDERED_TESTS = $(ORDERED_TESTS)"


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
