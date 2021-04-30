CITY_EXAMPLE_TOP = ..

.PHONY: all batch generate run trace demo minimal systematic \
		link test test-local clean clean-local real-clean


# Setting duration and scale:


# Licit durations are brief, short, medium and long:
CASE_DURATION := brief
#CASE_DURATION := short
#CASE_DURATION := medium
#CASE_DURATION := long


# Licit scales are: tiny, small, medium, large and huge:
CASE_SCALE := tiny
#CASE_SCALE := small
#CASE_SCALE := medium
#CASE_SCALE := large
#CASE_SCALE := huge


CASE_BASE_PARAMS := "--duration $(CASE_DURATION) --scale $(CASE_SCALE)"



# Setting how interactive the execution should be:

BATCH_PARAMS := "--batch $(CASE_BASE_PARAMS)"

TRACE_PARAMS := "$(CASE_BASE_PARAMS)"

MAX_PROCESS_OPT = --max-process-count 1000000


# Setting whether we want to develop and troubleshoot, or go for higher scale in
# production:
#
# Note: must be specified also at *compilation-time* (in addition to run-time)
#
# Ex: to really run in production mode, enter from the root of the sources:
# 'make clean all EXECUTION_TARGET=production'
#

# We define a local settings here, not to interfere with the rest of the build:
LOCAL_EXECUTION_TARGET := development
#LOCAL_EXECUTION_TARGET := production


HOST_FILE := "sim-diasca-host-candidates-for-scale-benchmarks.txt"

HOST_REF_FILE := "../../../sim-diasca/conf/sim-diasca-host-candidates.txt"


# Needed so that a standalone program (not a deployed simulation) is able to
# locate the City-Example main modules (ex: city_benchmarking_generation_test
# needing city_benchmarking):
#
BEAM_DIRS += $(CITY_EXAMPLE_TOP)/src


all:


# Case to generate in-memory (no initialisation file produced) and run (in a
# reproducible manner):
#
batch:
	@$(MAKE) -s city_benchmarking_run CMD_LINE_OPT="--batch --duration $(CASE_DURATION) --scale $(CASE_SCALE)" EXECUTION_TARGET=$(LOCAL_EXECUTION_TARGET)


# Case to generate an initialisation file:
#
# (each time an initialisation file with the same content is produced)
#
generate:
	@$(MAKE) -s city_benchmarking_generation_run CMD_LINE_OPT="--batch --duration $(CASE_DURATION) --scale $(CASE_SCALE)" EXECUTION_TARGET=$(LOCAL_EXECUTION_TARGET)


# Case to run a simulation using a previously-generated initialisation file:
#
# (using the same initialisation file shall result in exactly the same
# simulation)
#
run:
	@$(MAKE) -s city_benchmarking_loading_run CMD_LINE_OPT="--batch --duration $(CASE_DURATION) --scale $(CASE_SCALE)" EXECUTION_TARGET=$(LOCAL_EXECUTION_TARGET)



trace:
	@$(MAKE) -s city_benchmarking_run CMD_LINE_OPT="--duration $(CASE_DURATION) --scale $(CASE_SCALE)" EXECUTION_TARGET=$(LOCAL_EXECUTION_TARGET)


demo:
	@$(MAKE) -s city_benchmarking_run CMD_LINE_OPT="--duration brief --scale tiny" EXECUTION_TARGET=development


minimal:
	@$(MAKE) -s city_benchmarking_run CMD_LINE_OPT="--duration brief --scale tiny --batch" EXECUTION_TARGET=development



DURATIONS := brief short medium #long
#DURATIONS := brief

#SCALES := tiny small medium large #huge
SCALES := medium large #huge
#SCALES := medium



systematic:
	@$(MAKE) -s systematic-helper 2>&1 | tee $$(date '+%Y%m%d-%Hh%Mm%Ss')-city-example-systematic-testing.log


systematic-helper:
	@for s in $(SCALES) ; do for d in $(DURATIONS) ; do ( date ; printf "\n\n\n Testing for scale $$s and duration $$d" ; $(MAKE) -s city_benchmarking_generation_run CMD_LINE_OPT="--batch --duration $$d --scale $$s" EXECUTION_TARGET=production && date && $(MAKE) -s city_benchmarking_loading_run CMD_LINE_OPT="--batch --duration $$d --scale $$s" EXECUTION_TARGET=production ) ; done ; done


link:
	-@/bin/ln -s $(HOST_REF_FILE) $(HOST_FILE)


test: test-local

# Otherwise the loading case is run before the generating one:
# (minor side-effect: the generating case is run twice)
#
test-local: generate


clean: clean-local


# Removes all back-up files:
clean-local:
	-@/bin/rm -f Road_Network_2D_Topology.graph
	-@/bin/rm -f city-example-instances-version-*-scale-*.init-*


# Includes generated *.init files:
real-clean: clean
	-@/bin/rm -f city-example-instances-version-*-scale-*.init $(HOST_FILE)


include $(CITY_EXAMPLE_TOP)/GNUmakesettings.inc
