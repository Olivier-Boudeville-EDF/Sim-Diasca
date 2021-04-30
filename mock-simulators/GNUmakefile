MOCK_SIMULATORS_TOP = .


.PHONY: help help-intro help-mock-simulators                                   \
		register-version-in-header register-mock demo                          \
		clean-host-candidate-files generate-local-plt generate-local-plt-local


MODULES_DIRS = ssi-test soda-test city-example dataflow-urban-example doc


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


help: help-intro help-mock-simulators


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-mock-simulators:
	@cd $(SIM_DIASCA_TOP) && $(MAKE) -s help-sim-diasca



register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 54 ; else \
	$(MAKE) register-mock ; fi


register-mock:
	@echo "-define( mock_simulators_version, \"$(MOCK_SIMULATORS_VERSION)\" )." >> $(VERSION_FILE)


demo:
	@echo "Running a demo of all mock simulators"
	@cd soda-test/src && $(MAKE) -s demo
	@cd ssi-test//src && $(MAKE) -s demo
	@cd city-example/src && $(MAKE) -s demo
	@#cd dataflow-urban-example/src && $(MAKE) -s demo


stats:
	@$(MAKE_CODE_STATS) $(MOCK_SIMULATORS_TOP)



# Far simpler to duplicate and adapt the rule, rather then fixing all *_TOP
# variables, recursing and al:
#
# (could have been: '$(MAKE) -f $(SIM_DIASCA_TOP)/GNUmakefile $<')
clean-host-candidate-files:
	@find . -name "$(SIM_DIASCA_HOST_FILE)" -exec /bin/rm -f '{}' ';'


generate-local-plt: generate-local-plt-local

generate-local-plt-local:
	@echo "The 'generate-local-plt' target shall be run only on a per-simulator basis."
	@exit 5

include $(MOCK_SIMULATORS_TOP)/GNUmakesettings.inc
