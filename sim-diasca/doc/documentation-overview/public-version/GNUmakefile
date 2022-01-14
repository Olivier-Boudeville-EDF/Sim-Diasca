SIM_DIASCA_TOP = ../../..


.PHONY: all doc html


OVERALL_DOCUMENT_SOURCE = $(SD_DOC_OVERVIEW_PUBLIC_SRC)


# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


# Default do-nothing target:
all:
	@echo "  Run 'make html' to generate the "\
	"Sim-Diasca public documentation overview page."


doc: local-html local-pdf

html: local-html

pdf: local-pdf


# Regenerate RST source from template:
local-html: $(SD_DOC_OVERVIEW_PUBLIC_SRC) add-html-elements


add-html-elements:
	@echo "  Adding HTML-related elements"
	@for f in pygments-default.css sim-diasca.css; do ln -sf $(SIM_DIASCA_TOP)/doc/common-elements/css/$$f; done
	@for f in sim-diasca.png logo-EDF-english.png; do ln -sf $(SIM_DIASCA_TOP)/doc/common-elements/edf-related/$$f; done
	@ln -sf $(SIM_DIASCA_TOP)/doc/common-elements/licence/lgpl-v3-logo-bordered.png


deploy: html
	@echo "  Deploying public $(SD_DOC_OVERVIEW_NAME) to '$(DEPLOY_DIR)'."
	@/bin/cp -f *.png *.css *.html $(DEPLOY_DIR)
	@cd $(DEPLOY_DIR) && ln -s Sim-Diasca-public-documentation-overview.html index.html


clean: clean-generated


info-doc:
