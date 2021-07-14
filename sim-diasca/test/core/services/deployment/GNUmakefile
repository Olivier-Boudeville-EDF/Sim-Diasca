SIM_DIASCA_TOP = ../../../..

.PHONY: all deployment_reloading_saved_package_run test notify-error run-tests \
		run-test-forever run-tests-forever clean-local info-tests


all:


# From the simplest to the most demanding, to help the troubleshooting:
#
# (moreover there are a few dependencies between tests, for example the one
# saving a simulation archive should be run before the one loading it)
#
ORDERED_TESTS = \
	deployment_saving_package_test.erl                            \
	deployment_reloading_saved_package_test.erl                   \


# The package file must be available if testing its (re)loading:
deployment_reloading_saved_package_run: deployment_saving_package_run


# Note: as the 'test' target cannot be overridden, if the run-tests target is
# not commented, these tests will be run twice: one in the appropriate order
# (with 'run-tests') and a second one in the file order (anyway the needed files
# will already be available thanks to the dependency above).


# Some waiting is done between tests so that the VM can be shutdown on time:
test: #run-tests #notify-error

notify-error:
	@echo "Warning: the generic 'test' target should not be executed here, as it implies a run order basing on filenames, whereas we specified the reloading test to be executed after *the* saving one. Execute 'make run-tests' instead."
	@exit 1



run-tests:
	@echo "  Running all tests now in batch mode:"
	@for t in $(ORDERED_TESTS); do \
	target=$(echo $$t|sed 's|\_test.erl$$|_run|1'); \
	echo " ---- Running $$target ----"; \
	if ! $(MAKE) $$target CMD_LINE_OPT="--batch"; then \
	echo "######## Test $$target failed! ########" 1>&2; exit 120; \
	else sleep 1; echo; fi; done
	-@/bin/rm -f DeploymentSavingTest.sdar


clean: clean-local

clean-local:
	-@/bin/rm -f *.sdar


info-tests:
	@echo "ORDERED_TESTS = $(ORDERED_TESTS)"


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
