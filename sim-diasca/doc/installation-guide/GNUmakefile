SIM_DIASCA_TOP = ../..

.PHONY: all doc test clean clean-outputs real-clean


MODULES_DIRS = internal-version public-version


# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' in the relevant internal/public subdirectory to generate the installation guide."


doc:
	@for m in $(MODULES_DIRS); do (cd $$m && $(MAKE) -s doc); done


test:


clean:
	@for m in $(MODULES_DIRS); do (cd $$m && $(MAKE) -s clean); done


clean-outputs:


real-clean:
