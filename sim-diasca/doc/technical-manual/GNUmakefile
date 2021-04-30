SIM_DIASCA_TOP = ../..


.PHONY: all html pdf add-html-elements clean info-doc

OVERALL_DOCUMENT_SOURCE = $(SD_TECH_MANUAL_SRC)

HTML_TARGET = $(SD_TECH_MANUAL_HTML)

PROJECT_CSS := ../common-elements/css/pygments-default.css

# Gathers documentation elements from the root:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)

include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


# Default do-nothing target:
all:
	@echo "  Run 'make full-doc' to generate the technical "\
	"manual ($(SD_TECH_MANUAL_NAME))."


# Quick rules to generate the HTML documentation whereas the overall
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
	@echo "SD_TECH_MANUAL_NAME = $(SD_TECH_MANUAL_NAME)"
	@echo "SD_TECH_MANUAL_DIR = $(SD_TECH_MANUAL_DIR)"
	@echo "SD_TECH_MANUAL_BASE = $(SD_TECH_MANUAL_BASE)"
	@echo "SD_TECH_MANUAL_SRC = $(SD_TECH_MANUAL_SRC)"
	@echo "SD_TECH_MANUAL_HTML = $(SD_TECH_MANUAL_HTML)"
	@echo "SD_TECH_MANUAL_PDF = $(SD_TECH_MANUAL_PDF)"
