SIM_DIASCA_TOP = ../..


.PHONY: doc html pdf info-doc info-local-doc


OVERALL_DOCUMENT_SOURCE = $(SD_COUPLING_HOWTO_SRC)


# Default do-nothing target:
doc:
	@echo "  Run 'make local-doc' to generate the "\
	"coupling HOWTO ($(SD_COUPLING_HOWTO_BASE)) documents."


html: local-html

pdf: local-pdf


# Root specified as we want to catch the doc in src/core as well:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)

# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


info-doc: info-local-doc

info-local-doc:
	@echo "SD_COUPLING_HOWTO_NAME = $(SD_COUPLING_HOWTO_NAME)"
	@echo "SD_COUPLING_HOWTO_DIR = $(SD_COUPLING_HOWTO_DIR)"
	@echo "SD_COUPLING_HOWTO_BASE = $(SD_COUPLING_HOWTO_BASE)"
	@echo "SD_COUPLING_HOWTO_SRC = $(SD_COUPLING_HOWTO_SRC)"
	@echo "SD_COUPLING_HOWTO_HTML = $(SD_COUPLING_HOWTO_HTML)"
	@echo "SD_COUPLING_HOWTO_PDF = $(SD_COUPLING_HOWTO_PDF)"
