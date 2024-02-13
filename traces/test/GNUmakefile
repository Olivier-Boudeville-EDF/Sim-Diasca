TRACES_TOP = ..

.PHONY: all test-local prepare-otp-test

# To allow local tests (e.g. trace_bridging_test.erl) to depend on Myriad ones:
BEAM_DIRS += $(MYRIAD_TOP)/test/utils

# So that traces_otp_application_test reads my-traces-test.config:
INTERNAL_OPTIONS += --config my-traces-test

# So that it remains the default target:
all:

test: test-local

test-local: prepare-otp-test


prepare-otp-test:
	@cd $(TRACES_TOP) && $(MAKE) -s create-app-file


include $(TRACES_TOP)/GNUmakesettings.inc
