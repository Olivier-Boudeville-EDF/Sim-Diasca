SIM_DIASCA_TOP = ../..


.PHONY: all pdf

OVERALL_DOCUMENT_SOURCE = $(SD_DEV_GUIDE_SRC)

HTML_TARGET = $(SD_DEV_GUIDE_HTML)

PROJECT_CSS := ../common-elements/css/pygments-default.css

# Root specified as we want to catch the doc in src/core as well:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)

# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the "\
	"developer guide ($(SD_DEV_GUIDE_SRC))."


# Quick rules to generate the internal HTML documentation whereas the overall
# 'regenerate-doc' target has already been issued:

html: full-html

pdf: full-pdf


# Adds a local target:
create-html-doc: add-html-elements

add-html-elements:
	@echo "  Adding HTML-related elements"
	@cd $(TMP_RST_REPOSITORY) && (for f in pygments-default.css sim-diasca.css; do ln -sf ../$(SIM_DIASCA_TOP)/doc/common-elements/css/$$f; done)

clean: clean-doc-repository

info-doc:
	@echo "SD_INST_GUIDE_NAME = $(SD_INST_GUIDE_NAME)"
	@echo "SD_INST_GUIDE_DIR = $(SD_INST_GUIDE_DIR)"
	@echo "SD_INST_GUIDE_BASE = $(SD_DEV_GUIDE_BASE)"
	@echo "SD_INST_GUIDE_SRC = $(SD_DEV_GUIDE_SRC)"
	@echo "SD_INST_GUIDE_HTML = $(SD_DEV_GUIDE_HTML)"
	@echo "SD_INST_GUIDE_PDF = $(SD_DEV_GUIDE_PDF)"
