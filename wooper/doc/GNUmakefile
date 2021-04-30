WOOPER_TOP = ..


DOC_BASE_NAME := Ceylan-WOOPER-Layer-technical-manual-english


OVERALL_DOCUMENT_SOURCE := $(DOC_BASE_NAME).rst


CURRENT_WOOPER_HTML := $(DOC_BASE_NAME).html

HTML_TARGET := $(CURRENT_WOOPER_HTML)


CURRENT_WOOPER_PDF := $(DOC_BASE_NAME).pdf

PDF_TARGET := $(CURRENT_WOOPER_PDF)


DOC_GENERATED_FILES := $(HTML_TARGET) $(PDF_TARGET)



PROJECT_CSS := pygments-default.css,wooper.css

PROJECT_ICON := wooper-icon.png

WEB_CONTENT = *.html *.css *.png *.pdf

# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(WOOPER_WEB_MIRROR_BRANCH)")



.PHONY: doc all full-doc create-links html pdf clone-mirror         \
		export-doc export-to-official export-to-mirror              \
		clean clean-doc clean-links info info-web


# Default do-nothing target:
doc:


all:
	@echo "  Run 'make full-doc' to generate the manual of the 'WOOPER' layer."


full-doc: $(DOC_GENERATED_FILES)


# This link does not need to be in version control:
create-links:
	@/bin/ln -sf $(CURRENT_WOOPER_HTML) index.html


html: clean-doc $(HTML_TARGET)

pdf: clean-doc $(PDF_TARGET)


clone-mirror:
	@ echo "  Creating a separate repository in $(GITHUB_PAGES_BASE)"
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/Ceylan-WOOPER.git $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages
	@echo "Please remove all initial content in that selected 'gh-pages' branch."


export-doc: clean-doc full-doc export-to-official export-to-mirror


# Drawback: scp will not preserve symlinks, it will duplicate files instead.
#
# rsync would not, but requires a different source layout (copies a full tree,
# not a set of files), so currently not used:
# rsync -avze "ssh $(SP)" $(SOME_WEB_TREE) $(WEB_SRV):$(WEB_ROOT)/WOOPER/
#
export-to-official: create-links $(DOC_GENERATED_FILES)
	@echo "   Exporting WOOPER documentation to official website ($(WEB_SRV))"
	@/bin/scp $(SP) $(WEB_CONTENT) $(WEB_SRV):$(WEB_ROOT)/WOOPER/


export-to-mirror: create-links $(DOC_GENERATED_FILES)
	@echo "   Exporting WOOPER documentation to mirror website in $(WOOPER_WEB_MIRROR_BRANCH)"
	@/bin/cp -f $(WEB_CONTENT) $(WOOPER_WEB_MIRROR_BRANCH) && cd $(WOOPER_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "WOOPER doc updated." && git push && git pull --ff-only



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
	@echo "WOOPER_WEB_MIRROR_BRANCH = $(WOOPER_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


DOCUTILS_TOP = .

# For MYRIAD_TOP:
include $(WOOPER_TOP)/GNUmakesettings.inc
