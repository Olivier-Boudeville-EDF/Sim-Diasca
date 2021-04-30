SIM_DIASCA_TOP = ../../..


.PHONY: all html pdf add-html-elements clean info-doc

OVERALL_DOCUMENT_SOURCE = $(SD_INST_PUBLIC_GUIDE_SRC)

HTML_TARGET = $(SD_INST_PUBLIC_GUIDE_HTML)

PROJECT_CSS := $(SIM_DIASCA_TOP)/doc/common-elements/css/pygments-default.css

# Gathers documentation elements from the root:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)

# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the public installation "\
	"guide ($(SD_INST_PUBLIC_GUIDE_BASE))."


full-doc: regenerate-doc html pdf


# Quick rules to generate the public HTML documentation whereas the overall
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
	@echo "SD_INST_GUIDE_BASE = $(SD_INST_PUBLIC_GUIDE_BASE)"
	@echo "SD_INST_GUIDE_SRC = $(SD_INST_PUBLIC_GUIDE_SRC)"
	@echo "SD_INST_GUIDE_HTML = $(SD_INST_PUBLIC_GUIDE_HTML)"
	@echo "SD_INST_GUIDE_PDF = $(SD_INST_PUBLIC_GUIDE_PDF)"
