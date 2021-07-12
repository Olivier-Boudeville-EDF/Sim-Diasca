SIM_DIASCA_TOP = ../../..


.PHONY: doc html pdf secure-header-rst info-doc info-local-doc


OVERALL_DOCUMENT_SOURCE = $(SD_INST_PUBLIC_GUIDE_SRC)


# Default do-nothing target:
doc:
	@echo "  Run 'make full-doc' to generate the public installation "\
	"guide ($(SD_INST_PUBLIC_GUIDE_BASE)) documents."


html: secure-header-rst full-html

pdf: secure-header-rst full-pdf


secure-header-rst:
	@echo "  Securing common RST header"
	@cd .. && $(MAKE) -s SimDiasca-installation-guide-header-english.rst


# Root specified as we want to catch all relevant doc elements:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)/doc

# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


info-doc: info-local-doc


info-local-doc:
	@echo "SD_INST_GUIDE_NAME = $(SD_INST_GUIDE_NAME)"
	@echo "SD_INST_GUIDE_BASE_DIR = $(SD_INST_GUIDE_BASE_DIR)"
	@echo "SD_INST_PUBLIC_GUIDE_BASE = $(SD_INST_PUBLIC_GUIDE_BASE)"
	@echo "SD_INST_PUBLIC_GUIDE_SRC = $(SD_INST_PUBLIC_GUIDE_SRC)"
	@echo "SD_INST_PUBLIC_GUIDE_HTML = $(SD_INST_PUBLIC_GUIDE_HTML)"
	@echo "SD_INST_PUBLIC_GUIDE_PDF = $(SD_INST_PUBLIC_GUIDE_PDF)"
