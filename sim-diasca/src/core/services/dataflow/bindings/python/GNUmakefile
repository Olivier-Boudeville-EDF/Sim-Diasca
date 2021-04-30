SIM_DIASCA_TOP = ../../../../../..


.PHONY: generate-internal-api-src-archive generate-standalone-api-src-archive \
		generate-api-doc generate-api-doc-archive info-doc clean clean-doc

MODULES_DIRS = api

DOXYGEN := $$(which doxygen)

DOXYGEN_CONF := doxygen-for-python-api.conf

ZIP := $$(which zip)


# The *only* place where the API version is defined:
#
PYTHON_API_VERSION := 0.1.1


API_BASENAME := Sim-Diasca-Python-Dataflow-API-$(PYTHON_API_VERSION)


# Source section.

# There are two codebases for the implementation of the Python Dataflow API:
#
#  - the internal one, a part of coupling infrastructure (and thus not usable
#  without the rest of it)
#
#  - the standalone one, which emulates the whole coupling infrastructure (so
#  that processing units can be devised first just in its minimal Python
#  environment)
#
INTERNAL_SRC_ARCHIVE_BASENAME := $(API_BASENAME)-internal-sources
STANDALONE_SRC_ARCHIVE_BASENAME := $(API_BASENAME)-standalone-sources

INTERNAL_SRC_ARCHIVE_NAME := $(INTERNAL_SRC_ARCHIVE_BASENAME).zip
STANDALONE_SRC_ARCHIVE_NAME := $(STANDALONE_SRC_ARCHIVE_BASENAME).zip

INTERNAL_SRC_ARCHIVE_CONTENTS := api requirements.txt
STANDALONE_SRC_ARCHIVE_CONTENTS :=



# Documentation section.

# Actually based on the internal codebase:
DOC_ARCHIVE_BASENAME := $(API_BASENAME)-html-documentation

DOC_ARCHIVE_NAME := $(DOC_ARCHIVE_BASENAME).zip

DOC_PDF_NAME := $(API_BASENAME).pdf


SRC_ARCHIVE_EXCLUDED := *env* *__pycache__*

DOC_ARCHIVE_CONTENTS := $(ARCHIVE_BASENAME)



# Generates the archive of the sources of the (internal, not standalone)
# dataflow API:
#
generate-api-src-archive:
	@echo "    Generating the source archive of the internal Python Dataflow API, in $(INTERNAL_SRC_ARCHIVE_NAME)"
	@$(ZIP) $(INTERNAL_SRC_ARCHIVE_NAME) -r $(INTERNAL_SRC_ARCHIVE_CONTENTS) -x $(SRC_ARCHIVE_EXCLUDED)



# Generates the documentation of the API:
#
generate-api-doc:
	-@/bin/rm -rf latex html doxygen_sqlite3.db
	@( cat  $(DOXYGEN_CONF) ; echo "PROJECT_NUMBER=$(PYTHON_API_VERSION)" ) | $(DOXYGEN) - && echo -e "\n  The documentation of the Python Dataflow API has been generated, you can point your browser to file://$$PWD/html/index.html"


generate-api-doc-archive: generate-api-doc
	@echo -e "   Generating the documentation of the internal Python Dataflow API"
	-@/bin/rm -rf $(DOC_ARCHIVE_BASENAME)
	@/bin/mv html $(DOC_ARCHIVE_BASENAME)
	@$(ZIP) $(DOC_ARCHIVE_NAME) -r $(DOC_ARCHIVE_BASENAME)
	-@/bin/rm -rf $(DOC_ARCHIVE_BASENAME)
	@cd latex && make && /bin/mv -f refman.pdf ../$(DOC_PDF_NAME)
	-@/bin/rm -rf latex
	@echo -e "   Documentation of the internal Python Dataflow API generated:\n\t- HTML pages in $(DOC_ARCHIVE_NAME)\n\t- PDF in $(DOC_PDF_NAME)"



# -x $(ARCHIVE_EXCLUDED)

info-doc:
	@echo "  INTERNAL_SRC_ARCHIVE_NAME = $(INTERNAL_SRC_ARCHIVE_NAME)"
	@echo "  STANDALONE_SRC_ARCHIVE_NAME = $(STANDALONE_SRC_ARCHIVE_NAME)"
	@echo "  DOC_ARCHIVE_NAME = $(DOC_ARCHIVE_NAME)"
	@echo "  DOC_PDF_NAME = $(DOC_PDF_NAME)"


clean: clean-doc


clean-doc:
	@echo "   Cleaning the generated API documentation"
	-@/bin/rm -rf $(INTERNAL_SRC_ARCHIVE_NAME) $(STANDALONE_SRC_ARCHIVE_NAME) \
				  $(DOC_ARCHIVE_NAME) $(DOC_PDF_NAME) latex html \
				  doxygen_sqlite3.db


.PHONY: clean-local

include $(SIM_DIASCA_TOP)/GNUmakesettings.inc


clean: clean-local


clean-local:
	-@/bin/find . -name '__pycache__' -exec /bin/rm -rf '{}' ';' 2>/dev/null || true
