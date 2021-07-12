MOCK_SIMULATORS_TOP = ../../..


.PHONY: all test


# Allows to switch from batch mode to interactive one:
RUN_OPT := CMD_LINE_OPT="--batch"
#RUN_OPT :=

ALL_TESTS := $$(/bin/ls *test.erl | sed 's|test.erl|run|1')


DOC_SRC := unit-tests-overview.rst


all:

test: test-local

test-local:
	@for t in $(ALL_TESTS); do $(MAKE) $$t $(RUN_OPT) ; done


doc: doc-local


doc-local:
	@$(RST_GENERATOR) $(DOC_SRC)



info: info-tests


info-tests:
	@echo "ALL_TESTS = $(ALL_TESTS)"


include $(MOCK_SIMULATORS_TOP)/GNUmakesettings.inc
