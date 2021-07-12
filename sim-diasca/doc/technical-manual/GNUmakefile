SIM_DIASCA_TOP = ../..


.PHONY: doc html pdf info-doc info-local-doc

OVERALL_DOCUMENT_SOURCE = $(SD_TECH_MANUAL_SRC)


# Default do-nothing target:
doc:
	@echo "  Run 'make full-doc' to generate the technical "\
	"manual ($(SD_TECH_MANUAL_NAME))."


html: full-html

pdf: full-pdf


# Gathers documentation elements from the root:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)

# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


info-doc: info-local-doc

info-local-doc:
	@echo "SD_TECH_MANUAL_NAME = $(SD_TECH_MANUAL_NAME)"
	@echo "SD_TECH_MANUAL_DIR = $(SD_TECH_MANUAL_DIR)"
	@echo "SD_TECH_MANUAL_BASE = $(SD_TECH_MANUAL_BASE)"
	@echo "SD_TECH_MANUAL_SRC = $(SD_TECH_MANUAL_SRC)"
	@echo "SD_TECH_MANUAL_HTML = $(SD_TECH_MANUAL_HTML)"
	@echo "SD_TECH_MANUAL_PDF = $(SD_TECH_MANUAL_PDF)"
