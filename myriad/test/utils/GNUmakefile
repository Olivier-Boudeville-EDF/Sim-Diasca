MYRIAD_TOP = ../..

.PHONY: test-local prepare-otp-test


# So that it remains the default target:
all:

test: test-local

test-local: prepare-otp-test
	@$(MAKE) -s most_basic_example_exec


prepare-otp-test:
	@cd $(MYRIAD_TOP) && $(MAKE) -s rebar3-create-app-file

include $(MYRIAD_TOP)/GNUmakesettings.inc
