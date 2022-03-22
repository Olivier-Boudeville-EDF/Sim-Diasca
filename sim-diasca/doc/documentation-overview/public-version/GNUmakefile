SIM_DIASCA_TOP = ../../..


.PHONY: all doc local-doc html pdf local-html add-html-elements secure-extras clean


OVERALL_DOCUMENT_SOURCE = $(SD_DOC_OVERVIEW_PUBLIC_SRC)


# For Myriad's GNUmakerules-docutils.inc ultimately:
include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


# Default do-nothing target:
all:
	@echo "  Run 'make html' to generate the "\
	"Sim-Diasca public documentation overview page."


# A recursive 'doc' should not rebuild everything in all cases:
doc:

local-doc: local-html local-pdf


html: local-html

pdf: local-pdf


# Regenerate RST source from template:
local-html: $(SD_DOC_OVERVIEW_PUBLIC_SRC) add-html-elements


# Symlinks will be properly copied (as their content target) afterwards:
add-html-elements: secure-extras
	@echo "  Adding HTML-related elements"
	@for f in pygments-default.css sim-diasca.css; do ln -sf $(SIM_DIASCA_TOP)/doc/common-elements/css/$$f; done
	@for f in sim-diasca.png logo-EDF-english.png; do ln -sf $(SIM_DIASCA_TOP)/doc/common-elements/edf-related/$$f; done
	@ln -sf $(SIM_DIASCA_TOP)/doc/technical-manual/metaprogramming-with-sim-diasca.png .
	@ln -sf $(SIM_DIASCA_TOP)/doc/common-elements/licence/lgpl-v3-logo-bordered.png


# In a separate target, for more flexibilty and being able to avoid an execution
# in unwanted contexts (ex: when building the internal version):
#
secure-extras:
	@if [ -d "${SIM_DIASCA_INTERNAL_DOC_CLONE_ROOT}" ]; then ln -sf "${SIM_DIASCA_INTERNAL_DOC_CLONE_ROOT}/articles/ACME-application-IFL2021-article.pdf"; else echo "Error, IFL2021 article not found." 1>&2; exit 15; fi


clean: clean-generated
