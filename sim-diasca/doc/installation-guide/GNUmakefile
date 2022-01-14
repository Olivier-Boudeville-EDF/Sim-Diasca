SIM_DIASCA_TOP = ../..

.PHONY: all doc secure-doc-elements test clean clean-outputs real-clean


# Not defining MODULES_DIRS here, as not wanting to recurse in a possibly (for
# the public version) non-existing tree:
#
COND_MODULES_DIRS = internal-version public-version



# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' in the relevant internal/public subdirectory to generate the relevant version of the installation guide."


doc:
	@for m in $(COND_MODULES_DIRS); do if [ -d $$m ]; then (cd $$m && $(MAKE) -s doc); fi; done


# To be shared between public/internal.
#
# Header must be (re)-generated:
secure-doc-elements: Sim-Diasca-installation-guide-header-english.rst


test:


# Directory internal-version may not exist in public version:
clean:
	@for m in $(COND_MODULES_DIRS); do if [ -d $$m ]; then (cd $$m && $(MAKE) -s clean); fi; done


clean-outputs:


real-clean:


# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
