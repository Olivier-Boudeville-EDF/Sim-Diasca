SIM_DIASCA_TOP = ../..


.PHONY: doc html pdf info-doc info-local-doc


OVERALL_DOCUMENT_SOURCE = $(SD_DEV_GUIDE_SRC)


# Default do-nothing target:
doc:
	@echo "  Run 'make full-doc' to generate the "\
	"developer guide ($(SD_DEV_GUIDE_SRC)) documents."


html: full-html

pdf: full-pdf


# Root specified as we want to catch the doc in src/core as well:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)

# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


info-doc: info-local-doc

info-local-doc:
	@echo "SD_DEV_GUIDE_NAME = $(SD_DEV_GUIDE_NAME)"
	@echo "SD_DEV_GUIDE_DIR = $(SD_DEV_GUIDE_DIR)"
	@echo "SD_DEV_GUIDE_BASE = $(SD_DEV_GUIDE_BASE)"
	@echo "SD_DEV_GUIDE_SRC = $(SD_DEV_GUIDE_SRC)"
	@echo "SD_DEV_GUIDE_HTML = $(SD_DEV_GUIDE_HTML)"
	@echo "SD_DEV_GUIDE_PDF = $(SD_DEV_GUIDE_PDF)"
