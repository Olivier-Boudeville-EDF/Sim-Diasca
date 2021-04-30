TRACES_TOP = ..


DOC_BASE_NAME := Ceylan-Traces-Layer-technical-manual-english


OVERALL_DOCUMENT_SOURCE := $(DOC_BASE_NAME).rst


CURRENT_TRACES_HTML := $(DOC_BASE_NAME).html

HTML_TARGET := $(CURRENT_TRACES_HTML)


CURRENT_TRACES_PDF := $(DOC_BASE_NAME).pdf

PDF_TARGET := $(CURRENT_TRACES_PDF)


DOC_GENERATED_FILES := $(HTML_TARGET) $(PDF_TARGET)



PROJECT_CSS := pygments-default.css,traces.css

PROJECT_ICON := traces-icon.png

WEB_CONTENT = *.html *.css *.png *.pdf

# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(TRACES_WEB_MIRROR_BRANCH)")



.PHONY: doc all full-doc create-links html pdf clone-mirror         \
		export-doc export-to-official export-to-mirror              \
		clean clean-doc clean-links info info-web



# Default do-nothing target:
doc:


all:
	@echo "  Run 'make full-doc' to generate the manual of the 'Traces' layer."


full-doc: $(DOC_GENERATED_FILES)


# This link does not need to be in version control:
create-links:
	@/bin/ln -sf $(CURRENT_TRACES_HTML) index.html


html: clean-doc $(HTML_TARGET)

pdf: clean-doc $(PDF_TARGET)


# Creates a separate repository for the GitHub pages branch:
# (please then remove all initial content of that branch)
#
clone-mirror:
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/Ceylan-Traces.git $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages


export-doc: clean-doc full-doc export-to-official export-to-mirror


# Drawback: scp will not preserve symlinks, it will duplicate files instead.
#
# rsync would not, but requires a different source layout (copies a full tree,
# not a set of files), so currently not used:
# rsync -avze "ssh $(SP)" $(SOME_WEB_TREE) $(WEB_SRV):$(WEB_ROOT)/Traces/
#
export-to-official: create-links $(DOC_GENERATED_FILES)
	@echo "   Exporting Traces documentation to official website ($(WEB_SRV))"
	@/bin/scp $(SP) $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/Traces/


export-to-mirror: create-links $(DOC_GENERATED_FILES)
	@echo "   Exporting Traces documentation to mirror website in $(TRACES_WEB_MIRROR_BRANCH)"
	@/bin/cp -f $(WEB_CONTENT) $(TRACES_WEB_MIRROR_BRANCH) && cd $(TRACES_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "Traces doc updated." && git push && git pull --ff-only



clean: clean-doc


clean-doc: clean-links
	@echo "     Cleaning documentation"
	-@/bin/rm -f *.aux *.log *.maf *.mtc* *.stc* *.tex *.toc $(DOC_GENERATED_FILES)


clean-links:
	-@/bin/rm -f index.html


info: info-web


info-web:
	@echo "HTML_TARGET = $(HTML_TARGET)"
	@echo "SP = $(SP)"
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "WEB_SRV = $(WEB_SRV)"
	@echo "WEB_ROOT = $(WEB_ROOT)"
	@echo "TRACES_WEB_MIRROR_BRANCH = $(TRACES_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


DOCUTILS_TOP = .

# For MYRIAD_TOP:
include $(TRACES_TOP)/GNUmakesettings.inc
