TRACES_TOP = ..

.PHONY: all test-local prepare-otp-test

# To allow local tests (ex: trace_bridging_test.erl) to depend on Myriad ones:
BEAM_DIRS += $(MYRIAD_TOP)/test/utils

# So that it remains the default target:
all:

test: test-local

test-local: prepare-otp-test


prepare-otp-test:
	@cd $(TRACES_TOP) && $(MAKE) -s rebar3-create-app-file

include $(TRACES_TOP)/GNUmakesettings.inc
