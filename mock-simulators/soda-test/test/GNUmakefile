SODA_TEST_TOP = ..


.PHONY: all demo demo-batch batch run test-platform-integration


all:


demo:
	@$(MAKE) -s soda_stochastic_integration_run


demo-batch:
	@$(MAKE) -s soda_stochastic_integration_run CMD_LINE_OPT="--batch"


batch: demo-batch

run: demo


# Ex: make soda_stochastic_integration_run CMD_LINE_OPT="--trace-type pdf"


test-deterministic:
	@$(MAKE) -s soda_deterministic_integration_run CMD_LINE_OPT="--batch"
	@$(MAKE) -s soda_deterministic_integration_run


test-platform-integration:
	@$(MAKE) -s soda_platform_integration_run CMD_LINE_OPT="--batch -start-verbatim-options --simulation-instance-id 944"


include $(SODA_TEST_TOP)/GNUmakesettings.inc
