SIM_DIASCA_TOP = ../..

.PHONY: all doc test clean clean-outputs real-clean


MODULES_DIRS = internal-version public-version


# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' in the relevant internal/public subdirectory to generate the relevant version of the installation guide."


doc:
	@for m in $(MODULES_DIRS); do (cd $$m && $(MAKE) -s doc); done


# To be shared between public/internal.
#
# Header must be (re)-generated:
secure-doc-elements: SimDiasca-installation-guide-header-english.rst


test:


clean:
	@for m in $(MODULES_DIRS); do (cd $$m && $(MAKE) -s clean); done


clean-outputs:


real-clean:


# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
