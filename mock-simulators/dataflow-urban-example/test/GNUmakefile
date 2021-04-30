DATAFLOW_URBAN_EXAMPLE_TOP = ..


.PHONY: all batch run demo prog-batch prog-run                             \
		vehicle vcl-prog-batch vcl-prog-run                                \
		emul-batch emul-run python-and-unit-manager-batch                  \
		stats test-all-cases info-cases clean-test-logs


MODULES_DIRS = unit-tests


# Allows to switch from batch mode to interactive one:
RUN_OPT := CMD_LINE_OPT="--batch"
#RUN_OPT :=

# Disabled, waited for the Java binding to be finished:
#ALL_CASES := $$(/bin/ls *case.erl | sed 's|case.erl|run|1')
ALL_CASES := dataflow_urban_example_platform_emulating_run \
			 dataflow_urban_example_programmatic_with_dumf_mockup_vehicle_run  \
			 dataflow_urban_example_platform_emulating_with_python_vehicle_run \
			 dataflow_urban_example_programmatic_run                           \
			 dataflow_urban_example_programmatic_with_native_vehicle_run       \
			 dataflow_urban_example_programmatic_with_python_vehicle_run

#			 dataflow_urban_example_programmatic_with_java_vehicle_run
#			 dataflow_urban_example_programmatic_with_all_vehicle_types_run    \


all:


batch:
	@$(MAKE) -s vehicle RUN_OPT=CMD_LINE_OPT="--batch"

run:
	@$(MAKE) -s vehicle RUN_OPT=


demo:
	@$(MAKE) -s vcl-prog-overall RUN_OPT=


vehicle: vehicle-prog vehicle-emul


vehicle-prog: vcl-prog-native vcl-prog-python vcl-prog-java vcl-prog-dumf \
			  vcl-prog-overall


vcl-prog-native:
	@$(MAKE) dataflow_urban_example_programmatic_with_native_vehicle_run $(RUN_OPT)


vcl-prog-python:
	@$(MAKE) dataflow_urban_example_programmatic_with_python_vehicle_run $(RUN_OPT)


vcl-prog-java: VehicleTypeUnit.class
	@$(MAKE) dataflow_urban_example_programmatic_with_java_vehicle_run $(RUN_OPT)


vcl-prog-dumf:
	@$(MAKE) dataflow_urban_example_programmatic_with_dumf_mockup_vehicle_run $(RUN_OPT)

vcl-prog-overall:
	@$(MAKE) dataflow_urban_example_programmatic_with_all_vehicle_types_run $(RUN_OPT)


vehicle-emul: vcl-emul-native vcl-emul-python


vcl-emul-native:
	@$(MAKE) dataflow_urban_example_platform_emulating_run $(RUN_OPT)

vcl-emul-python:
	@$(MAKE) dataflow_urban_example_platform_emulating_with_python_vehicle_run $(RUN_OPT)


test-all-cases:
	@echo "    Testing all registered dataflow simulation cases"
	@for case in $(ALL_CASES); do echo " - testing $$case" ; $(MAKE) $$case $(RUN_OPT) 2>&1 | tee $$(date '+%Y%m%d-%Hh%Mm%Ss')-$$case.log ; done


stats:
	@$(MAKE_CODE_STATS) $(PWD)


info: info-cases


info-cases:
	@echo "ALL_CASES = $(ALL_CASES)"


clean: clean-test-logs
	-@/bin/rm -f *.log



include $(DATAFLOW_URBAN_EXAMPLE_TOP)/GNUmakesettings.inc
