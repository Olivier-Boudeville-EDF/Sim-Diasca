SIM_DIASCA_TOP = ../../../../../..


.PHONY: test-plot-generation clean


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


Test_probe.dat:
	@$(MAKE) -s class_Probe_run


test-plot-generation:
	@$(MAKE) && $(ERLANG_INTERPRETER) -run generate_plots_for_time_series run .


clean: clean-local


clean-local:
	-@/bin/rm -f Basic_Test_probe.*  Zone_Test_probe.*
