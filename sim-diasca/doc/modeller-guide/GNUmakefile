SIM_DIASCA_TOP = ../..


.PHONY: doc html pdf doc-local clean clean-local info-doc info-local-doc



OVERALL_DOCUMENT_SOURCE = $(SD_MOD_GUIDE_SRC)

ROOT_TOP := $(SIM_DIASCA_TOP)/..


# Default do-nothing target:
doc:
	@echo "  Run 'make full-doc' to generate the "\
	"modeller guide ($(SD_MOD_GUIDE_SRC)) documents."


html: doc-local full-html

pdf: doc-local full-pdf


# We need to rely on images generated in other layers (ex: sim-diasca for
# FSM-legend.png, mock-simulators for soda-ordering-interacting-FSM.png):
#
# (we do not add their symlink in GIT as it would be dead as long as the PNG is
# not generated, and this would block the release preparation that attempts to
# follow symlinks; hence we recreate them iff needed)
#
doc-local:
	-@ln -sf $(ROOT_TOP)/mock-simulators/soda-test/doc/soda-ordering-interacting-FSM.png
	@cd $(SIM_DIASCA_TOP) && $(MAKE) -s doc
	@cd $(ROOT_TOP)/mock-simulators && $(MAKE) -s doc


clean: clean-local

# Just a link:
clean-local:
	-@/bin/rm -f soda-ordering-interacting-FSM.png


# Root specified as we want to catch the doc in src/core as well:
DOCUTILS_TOP = $(SIM_DIASCA_TOP)

# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


info-doc: info-local-doc

info-local-doc:
	@echo "SD_MOD_GUIDE_NAME = $(SD_MOD_GUIDE_NAME)"
	@echo "SD_MOD_GUIDE_DIR = $(SD_MOD_GUIDE_DIR)"
	@echo "SD_MOD_GUIDE_BASE = $(SD_MOD_GUIDE_BASE)"
	@echo "SD_MOD_GUIDE_SRC = $(SD_MOD_GUIDE_SRC)"
	@echo "SD_MOD_GUIDE_HTML = $(SD_MOD_GUIDE_HTML)"
	@echo "SD_MOD_GUIDE_PDF = $(SD_MOD_GUIDE_PDF)"
