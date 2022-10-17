MYRIAD_TOP = ..


DOC_BASE_NAME := Ceylan-Myriad-Layer-technical-manual-english


OVERALL_DOCUMENT_SOURCE := $(DOC_BASE_NAME).rst


CURRENT_MYRIAD_HTML := $(DOC_BASE_NAME).html

HTML_TARGET := $(CURRENT_MYRIAD_HTML)


CURRENT_MYRIAD_PDF := $(DOC_BASE_NAME).pdf

PDF_TARGET := $(CURRENT_MYRIAD_PDF)


DOC_GENERATED_FILES := $(HTML_TARGET) $(PDF_TARGET)



PROJECT_CSS := pygments-default.css,myriad.css

PROJECT_ICON := myriad-icon.png


# Read from the user's environment:
GITHUB_PAGES_BASE := $(shell basename "$(MYRIAD_WEB_MIRROR_BRANCH)")


# Additional to WEB_CONTENT:
EXTRA_CONTENT := speech-test-fr-FR.ogg.opus speech-test-en-US.ogg.opus


.PHONY: all doc html pdf create-links clone-mirror force-update        \
		export-doc export-doc-to-official export-doc-to-mirror         \
		info info-web                                                  \
		test uml-diagram-test



# Default do-nothing target:
all:
	@echo "  Run 'make doc' to generate the documentation of the 'Myriad' layer."


doc: html pdf create-api-doc


html: local-html

pdf: local-pdf


# This link does not need to be in version control:
create-links:
	@/bin/ln -sf $(CURRENT_MYRIAD_HTML) index.html



# Creates a separate repository for the GitHub pages branch:
clone-mirror:
	@echo "  Creating a separate repository for website in $(GITHUB_PAGES_BASE)"
	@cd ../../ && git clone https://github.com/Olivier-Boudeville/Ceylan-Myriad.git $(GITHUB_PAGES_BASE) && cd $(GITHUB_PAGES_BASE) && git checkout -b gh-pages
	@echo "Please remove all initial content in that selected 'gh-pages' branch."


# Forces the update of the overall documentation structure (notably in order to
# update its timestamp), knowing that, otherwise, modifying source RST files
# that it references would not update it in turn):
#
# Removing the generated main .rst, not its .rst.template real source:
force-update:
	@/bin/rm -f $(OVERALL_DOCUMENT_SOURCE)


export-doc: export-doc-to-official export-doc-to-mirror


# Drawback: scp will not preserve symlinks, it will duplicate files instead
# (which may be useful though, for example for MathJax).
#
# rsync would not, but requires a different source layout (copies a full tree,
# not a set of files), so currently not used:
# rsync -avze "ssh $(SP)" $(SOME_WEB_TREE) $(WEB_SRV):$(WEB_ROOT)/Myriad/
#
export-doc-to-official: check-mathjax clean-doc create-links doc
	@echo "   Exporting Myriad documentation to official website ($(WEB_SRV))"
	@/bin/scp $(SP) -r $(WEB_CONTENT) $(EXTRA_CONTENT) $(WEB_SRV):$(WEB_ROOT)/Myriad/


# H option for 'cp', so that a local MathJax is copied (not symlinked to a local
# directory) in target branch:
#
export-doc-to-mirror: check-mathjax clean-doc create-links doc
	@echo "   Exporting Myriad documentation to mirror website in $(MYRIAD_WEB_MIRROR_BRANCH)"
	@/bin/cp -rfH $(WEB_CONTENT) $(EXTRA_CONTENT) $(MYRIAD_WEB_MIRROR_BRANCH) && cd $(MYRIAD_WEB_MIRROR_BRANCH) && git add . && git merge -s ours && git commit -m "Myriad doc updated." && git push && git pull --ff-only



info: info-doc info-web


info-web:
	@echo "HTML_TARGET = $(HTML_TARGET)"
	@echo "SP = $(SP)"
	@echo "WEB_CONTENT = $(WEB_CONTENT)"
	@echo "EXTRA_CONTENT = $(EXTRA_CONTENT)"
	@echo "WEB_SRV = $(WEB_SRV)"
	@echo "WEB_ROOT = $(WEB_ROOT)"
	@echo "MYRIAD_WEB_MIRROR_BRANCH = $(MYRIAD_WEB_MIRROR_BRANCH)"
	@echo "GITHUB_PAGES_BASE = $(GITHUB_PAGES_BASE)"


# As a side-effect, early checking that the 'dot' tool will be available.
#
# Finally disabled, as continuous integration does not have such tools by
# default.
#
#test: uml-diagram-test
test:

uml-diagram-test:
	@$(MAKE) uml_class_diagram_example.png


DOCUTILS_TOP = .

include $(MYRIAD_TOP)/GNUmakesettings.inc
