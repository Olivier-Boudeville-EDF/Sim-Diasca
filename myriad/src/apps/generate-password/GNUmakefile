MYRIAD_TOP = ../../..

.PHONY: make-test-tree test-local test-local-scan run-test-tree \
		debug run-direct clean-test-tree

GEN_SCRIPT := ./generate-password.escript


all:



run-direct: generate_password_exec


run-param:
	@$(MAKE) generate_password_exec CMD_LINE_OPT="-h"


generate_password_exec: password_generation.beam


help:
	@$(GEN_SCRIPT) --help


clean:


include $(MYRIAD_TOP)/GNUmakesettings.inc
