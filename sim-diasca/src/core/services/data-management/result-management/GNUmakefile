SIM_DIASCA_TOP = ../../../../..


.PHONY: all test-web test-web-batch test-web-interactive \
		pre-test clean clean-probes


all:

test-web: test_web-batch


test-web-batch: pre-test
	@$(MAKE) -s web_result_management_run $$BATCH


test-web-interactive: pre-test
	@$(MAKE) -s web_result_management_run


# Avoid any lingering Node.js / Java (for LogMX) instance:
pre-test: clean-probes
	-@killall /usr/bin/node java 1>/dev/null 2>&1 || true


clean: clean-probes


clean-probes:
	@echo "   Cleaning web probes"
	-@/bin/rm -f web-probe-*.html


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
