SIM_DIASCA_TOP = ../../../../../..


.PHONY: all test-plot-generation clean clean-local


all:

Test_probe.dat:
	@$(MAKE) -s class_Probe_run


test-plot-generation:
	@$(MAKE) && $(ERLANG_INTERPRETER) -run generate_plots_for_time_series run .


clean: clean-local


clean-local:
	-@/bin/rm -f Basic_Test_probe.*  Zone_Test_probe.*



include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
