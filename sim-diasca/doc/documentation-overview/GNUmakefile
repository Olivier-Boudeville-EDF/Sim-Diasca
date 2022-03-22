SIM_DIASCA_TOP = ../..

.PHONY: all doc test clean clean-outputs real-clean info-doc info-doc-overview


# Not defining MODULES_DIRS here, as not wanting to recurse in a possibly (for
# the public version) non-existing tree:
#
COND_MODULES_DIRS = internal-version public-version



# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' in the relevant internal/public subdirectory to generate the relevant version of the installation guide."


doc:
	@for m in $(COND_MODULES_DIRS); do if [ -d $$m ]; then (cd $$m && $(MAKE) -s doc); fi; done



test:


# Directory internal-version may not exist in public version:
clean:
	@for m in $(COND_MODULES_DIRS); do if [ -d $$m ]; then (cd $$m && $(MAKE) -s clean); fi; done


clean-outputs:


real-clean:


info-doc: info-doc-overview


info-doc-overview:
	@echo "SD_DOC_OVERVIEW_NAME = $(SD_DOC_OVERVIEW_NAME)"
	@echo "SD_DOC_OVERVIEW_BASE_DIR = $(SD_DOC_OVERVIEW_BASE_DIR)"
	@echo "SD_DOC_OVERVIEW_INTERNAL_DIR = $(SD_DOC_OVERVIEW_INTERNAL_DIR)"
	@echo "SD_DOC_OVERVIEW_PUBLIC_DIR = $(SD_DOC_OVERVIEW_PUBLIC_DIR)"
	@echo "SD_DOC_OVERVIEW_INTERNAL_BASE = $(SD_DOC_OVERVIEW_INTERNAL_BASE)"
	@echo "SD_DOC_OVERVIEW_PUBLIC_BASE = $(SD_DOC_OVERVIEW_PUBLIC_BASE)"
	@echo "SD_DOC_OVERVIEW_INTERNAL_TMPL = $(SD_DOC_OVERVIEW_INTERNAL_TMPL)"
	@echo "SD_DOC_OVERVIEW_PUBLIC_TMPL = $(SD_DOC_OVERVIEW_PUBLIC_TMPL)"
	@echo "SD_DOC_OVERVIEW_INTERNAL_SRC = $(SD_DOC_OVERVIEW_INTERNAL_SRC)"
	@echo "SD_DOC_OVERVIEW_PUBLIC_SRC = $(SD_DOC_OVERVIEW_PUBLIC_SRC)"
	@echo "SD_DOC_OVERVIEW_INTERNAL_HTML = $(SD_DOC_OVERVIEW_INTERNAL_HTML)"
	@echo "SD_DOC_OVERVIEW_PUBLIC_HTML = $(SD_DOC_OVERVIEW_PUBLIC_HTML)"
	@echo "SD_DOC_OVERVIEW_INTERNAL_PDF = $(SD_DOC_OVERVIEW_INTERNAL_PDF)"
	@echo "SD_DOC_OVERVIEW_PUBLIC_PDF = $(SD_DOC_OVERVIEW_PUBLIC_PDF)"



# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
