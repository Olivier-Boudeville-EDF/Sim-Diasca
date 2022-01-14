# OVERALL_DOCUMENT_SOURCE, MYRIAD_TOP and DOCUTILS_TOP should have been defined
# beforehand.



.PHONY: doc doc-recurse info info-doc-tools info-doc-settings local full-doc   \
 create-doc create-html-doc create-pdf-doc full-html full-pdf regenerate-doc   \
 local-doc local-html local-pdf create-api-doc                                 \
 make-doc-repository create-local-referential                                  \
 create-mathjax-symlink check-mathjax                                          \
 view-doc to-print check-dot                                                   \
 info-doc info-doc-local info-doc-tools                                        \
 clean clean-doc-repository clean-generated clean-dot clean-doc-logs           \
 clean-gnuplot clean-png clean-rst


# Content typically to be deployed to webservers:
#
# /bin/ls is used so that any lacking element in a layer (ex: no api-doc
# generatedn, no MathJax available) does not trigger an error.
#
WEB_CONTENT = $$(/bin/ls -d *.html *.css *.png *.pdf api-doc mathjax 2>/dev/null)


# Generic rules.



# Image section.

#IMG_VIEWER := $(which xv)
IMG_VIEWER := $$(which eog)

IMAGE_MAGICK := $$(which convert)


# Not all python scripts generate images:
PNG_FROM_PYTHON = $(patsubst %-png-generator.py,%.png,$(wildcard *-png-generator.py))


PNG_FROM_PYTHON_NEGATED = $(patsubst %-png-generator.py,%-negated.png,$(wildcard *-png-generator.py))


NEGATED_PNG = $(patsubst %.png,%-negated.png,$(wildcard *.png))

%-negated.png: %.png
	@echo "    Generating $@ from $< with $(IMAGE_MAGICK)"
	@$(IMAGE_MAGICK) $< -negate $@



ROTATED_PNG = $(patsubst %.png,%-rotated.png,$(wildcard *.png))

# '-transpose' is not what we want:
%-rotated.png: %.png
	@echo "    Generating $@ from $< with $(IMAGE_MAGICK)"
	@$(IMAGE_MAGICK) $< -rotate -90 $@



BORDERED_PNG = $(patsubst %.png,%-bordered.png,$(wildcard *.png))

# Adds to the image a thin black border enclosing some additional white space:
%-bordered.png: %.png
	@echo "    Generating $@ from $< with $(IMAGE_MAGICK)"
	@$(IMAGE_MAGICK) $< -mattecolor White -frame 4x4 -mattecolor Black -frame 1x1 $@



# Graph rendering section.


# Dot must found directly from the environment, since it is needed by
# both the Make system and the Erlang application.
# DOT := $$(PATH=$(DOT_ROOT)/bin:$$PATH which dot)
DOT := $$(which dot)

DOT_OPT := -Gcharset=latin1

# To trigger the automatic rendering of a created graph, add 'VIEW_GRAPH='
# at the end of the command line (ex : 'make MyFSM.png VIEW_GRAPH=')
VIEW_GRAPH := no
#VIEW_GRAPH :=


# Often used: 'make foo.png && eog foo.png' to force display even if already
# generated:
#
%.png: %.graph
	@echo "     Generating $@ from graph $<"
	@$(MAKE) -s check-dot
	@if $(DOT) $(DOT_OPT) -o$@ -Tpng $<; then if [ "$(VIEW_GRAPH)" != "no" ]; then $(IMG_VIEWER) $@; fi; fi


GRAPH_SRC = $(wildcard *.graph)
GRAPH_PNG = $(GRAPH_SRC:%.graph=%.png)



# Plot rendering section.

%.dat: %.py
	@echo "     Generating plot data $@ from $<"
	@$(PYTHON) $<

# To trigger the automatic rendering of a created plot, add 'VIEW_PLOT='
# at the end of the command line (ex : 'make MyData.png VIEW_PLOT=')
#VIEW_PLOT := yes
VIEW_PLOT := no

GNUPLOT := $$(which gnuplot)

%.png: %.plot %.dat
	@echo "     Generating plot $@"
	@$(GNUPLOT) $<
	@if [ "$(VIEW_PLOT)" != "no" ]; then $(IMG_VIEWER) $@ & fi


# Some plots do not need data files:
%.png: %.plot
	@echo "     Generating plot $@"
	@$(GNUPLOT) $<
	@if [ "$(VIEW_PLOT)" != "no" ]; then $(IMG_VIEWER) $@ & fi


%.png: %.py
	@echo "     Generating image $@ from $<"
	@$(PYTHON) $<
	@if [ "$(VIEW_PLOT)" != "no" ]; then $(IMG_VIEWER) $@ & fi


# Probably deprecated:
%-generated.png: %.pdoc %.plotdoc
	@echo "     Generating plot $@"
	@$(GNUPLOT) $<
	@if [ "$(VIEW_PLOT)" != "no" ]; then $(IMG_VIEWER) $@ & fi




# RST section.

GENERATOR_LOG_FILE := "rst.log"

TMP_RST_REPOSITORY := "tmp-rst"


# OVERALL_DOCUMENT_SOURCE defined in per-project GNUmakefile:
OVERALL_DOCUMENT_HTML_TARGET = $(patsubst %.rst,%.html,$(OVERALL_DOCUMENT_SOURCE))
OVERALL_DOCUMENT_PDF_TARGET = $(patsubst %.rst,%.pdf,$(OVERALL_DOCUMENT_SOURCE))


RST_TEMPLATE_FILES = $(wildcard *.rst.template)
RST_FILES = $(wildcard *.rst)

AUX_GENERATED_FILES  = $(patsubst %.rst,%.aux,$(RST_FILES))
TEX_GENERATED_FILES  = $(patsubst %.rst,%.tex,$(RST_FILES))
OUT_GENERATED_FILES  = $(patsubst %.rst,%.out,$(RST_FILES))
LOG_GENERATED_FILES  = $(patsubst %.rst,%.log,$(RST_FILES))
TOC_GENERATED_FILES  = $(patsubst %.rst,%.toc,$(RST_FILES))

HTML_GENERATED_FILES = $(patsubst %.rst,%.html,$(RST_FILES)) \
					   $(patsubst %.rst.template,%.html,$(RST_TEMPLATE_FILES))

PDF_GENERATED_FILES  = $(patsubst %.rst,%.pdf,$(RST_FILES)) \
					   $(patsubst %.rst.template,%.pdf,$(RST_TEMPLATE_FILES))


RST_GENERATED_FILES = $(AUX_GENERATED_FILES) $(TEX_GENERATED_FILES)       \
	$(OUT_GENERATED_FILES) $(LOG_GENERATED_FILES) $(TOC_GENERATED_FILES)  \
	$(HTML_GENERATED_FILES) $(PDF_GENERATED_FILES) $(GENERATOR_LOG_FILE)


#RST_GENERATOR = update-docutils.sh
RST_GENERATOR := $$(PATH=$(MYRIAD_TOP)/src/scripts:$$PATH which generate-docutils.sh)


PDF_VIEWER := $$(which evince)


# Generates the actual, up-to-date HTML page corresponding to the specified
# template (typically useful for the documentation main pages):
#
%.rst: %.rst.template
	@echo "  Generating $@ from $<"
	@cat $< | sed "s|PROJECT_VERSION_TAG|$(PROJECT_VERSION)|g" | sed "s|GENERATION_DATE_TAG|$$(date '+%A, %B %-e, %Y')|g" | sed "s|GENERATION_YEAR_TAG|$$(date '+%Y')|g" > $@


# To trigger the automatic rendering of a created PDF, add 'VIEW_PDF='
# at the end of the command line (ex : 'make MyDoc.pdf VIEW_PDF=')
#VIEW_PDF := yes
VIEW_PDF := no


%.pdf: %.rst
	@echo "     Generating PDF documentation $@"
	@if $(RST_GENERATOR) $< --pdf; then if [ "$(VIEW_PDF)" != "no" ]; then $(PDF_VIEWER) $@ & fi; else exit 5; fi


%.html: %.rst
	@echo "     Generating HTML documentation $@"
	@$(RST_GENERATOR) $< $(PROJECT_CSS) $(PROJECT_ICON_OPT)


%.pdf: %.traces
	@echo "     Generating PDF documentation $@ from $<"
	@if $(RST_GENERATOR) $< --pdf; then if [ "$(VIEW_PDF)" != "no" ]; then $(PDF_VIEWER) $@ & fi; else exit 15; fi



# Generates a module-specific documentation, typically from the directory in
# which that module is defined.
#
# Ex: make ../../doc/api-doc/app_facilities.html
#
$(API_DOC_ROOT)/%.html: %.erl
	@echo "    Generating edoc documentation for module '$<' in $@"
	@$(ERLANG_INTERPRETER) -noshell -run edoc_run file '"$<"' $(EDOC_OPTS)



# Basic rules.



doc: $(GRAPH_PNG) $(PNG_FROM_PYTHON) $(PNG_FROM_PYTHON_NEGATED) doc-recurse #$(PDF_GENERATED_FILES)


clean: clean-doc

clean-doc: clean-links clean-doc-repository
	@echo "     Cleaning documentation elements in $$(basename $$(pwd))"
	-@/bin/rm -f *.aux *.log *.maf *.mtc* *.stc* *.tex *.toc $(RST_GENERATED_FILES)
	-@/bin/rm -rf api-doc


clean-links:
	-@/bin/rm -f index.html


# Not info-doc-local, as always left blank so that it can be defined:
info-doc: info-doc-tools info-doc-settings

info-doc-tools:
	@echo "RST_GENERATOR = $(RST_GENERATOR)"
	@echo "DOC_TRACKER = $(DOC_TRACKER)"
	@echo "DOT = $(DOT)"
	@echo "GNUPLOT = $(GNUPLOT)"
	@echo "IMG_VIEWER = $(IMG_VIEWER)"
	@echo "IMAGE_MAGICK = $(IMAGE_MAGICK)"
	@echo "PDF_VIEWER = $(PDF_VIEWER)"


info-doc-settings:
	@echo "OVERALL_DOCUMENT_SOURCE = $(OVERALL_DOCUMENT_SOURCE)"
	@echo "OVERALL_DOCUMENT_HTML_TARGET = $(OVERALL_DOCUMENT_HTML_TARGET)"
	@echo "OVERALL_DOCUMENT_PDF_TARGET = $(OVERALL_DOCUMENT_PDF_TARGET)"
	@echo "PRINT_SERVER = $(PRINT_SERVER)"
	@echo "WEB_CONTENT = $(WEB_CONTENT)"



# Centralisation of documentation sources is necessary, as the LaTeX generator
# from docutils will not find images in directories otherwise.
#
# Note: cannot use a 'doc' target, as it would result in an infinite recursion.
#
full-doc: create-doc


# The documentation source is also listed among the prerequisites, as this X.rst
# might be generated from a X.rst.template file:
#
create-doc: $(OVERALL_DOCUMENT_SOURCE) regenerate-doc clean-doc-repository \
		make-doc-repository
	@echo "     Generating full documentation from \
	$(OVERALL_DOCUMENT_SOURCE) in $(TMP_RST_REPOSITORY) \
	(log in $(GENERATOR_LOG_FILE))"
	@$(MAKE) -s create-html-doc create-pdf-doc



# Centralisation for lighter documentation that can be generated from the
# current tree only:
#
# (better not define generically html/pdf, but only {local,full}-{html,pdf} so
#  that, on a per-document basis, each makefile may select local or full rules)
#
local-doc: local-html local-pdf


local-html: $(OVERALL_DOCUMENT_SOURCE)
	@echo "     Generating HTML documentation from $(OVERALL_DOCUMENT_SOURCE)"
	@$(RST_GENERATOR) $(OVERALL_DOCUMENT_SOURCE) $(PROJECT_CSS) $(PROJECT_ICON_OPT)


local-pdf: $(OVERALL_DOCUMENT_SOURCE)
	@echo "     Generating PDF documentation from $(OVERALL_DOCUMENT_SOURCE)"
	@if $(RST_GENERATOR) $(OVERALL_DOCUMENT_SOURCE) --pdf; then if [ "$(VIEW_PDF)" != "no" ]; then $(PDF_VIEWER) $(OVERALL_DOCUMENT_PDF_TARGET) & fi; else exit 15; fi


# To generate an actual API documentation tree:
create-api-doc:
	@cd $(LAYER_TOP) && $(MAKE) -s generate-api-doc


# As no Makefile in TMP_RST_REPOSITORY:

create-html-doc:
	@echo "  Generating $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_HTML_TARGET)"
	@$(MAKE) -s $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_HTML_TARGET) 2>&1 | \
	tee $(GENERATOR_LOG_FILE)


create-pdf-doc:
	@echo "  Generating $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_PDF_TARGET)"
	@$(MAKE) -s $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_PDF_TARGET) 2>&1 | \
	tee $(GENERATOR_LOG_FILE)



# Quick rule to generate the HTML documentation whereas the overall
# 'regenerate-doc' target has already been issued.
#
full-html: clean-doc-repository make-doc-repository create-html-doc


# Quick rule to generate the PDF documentation whereas the overall
# 'regenerate-doc' target has already been issued.
#
full-pdf: clean-doc-repository make-doc-repository create-pdf-doc



regenerate-doc:
	@echo "     Regenerating all documentation elements from $(DOCUTILS_TOP)"
	@cd $(DOCUTILS_TOP) && $(MAKE) -s doc


make-doc-repository:
	@echo "     Copying documentation sources to $(TMP_RST_REPOSITORY)"
	@mkdir -p $(TMP_RST_REPOSITORY)
	@find $(DOCUTILS_TOP) -name $(TMP_RST_REPOSITORY) -prune -o \( -name '*.rst.template' -o -name '*.rst' -o -name '*.png' -o -name '*.css' \) -exec /bin/cp -f '{}' $(TMP_RST_REPOSITORY) ';'



MATHJAX_ROOT="/usr/share/mathjax"

# In order to generate beautiful math formulas for the HTML output; here a local
# MathJax install is expected.
#
# See myriad/src/scripts/generate-docutils.sh for more details.
#
create-mathjax-symlink:
	@if [ -e "${MATHJAX_ROOT}" ]; then ln -sf "${MATHJAX_ROOT}"; else echo "Error, no pre-existing MathJax install found (no '${MATHJAX_ROOT}'; consider installation with 'pacman -Sy mathjax' or alike)." 1>&2 ; exit 75; fi


check-mathjax:
	@if [ ! -e "${MATHJAX_ROOT}" ]; then echo "Error, no MathJax install found (no '${MATHJAX_ROOT}'; consider installation with 'pacman -Sy mathjax' or alike)." 1>&2 ; exit 75; fi



create-local-referential: clean-doc create-links doc
	@echo "   Creating a local $(PROJECT_NAME) documentation referential in $(DOC_TARGET_DIR)"
	@if [ -z "$(DOC_TARGET_DIR)" ]; then echo "  Error, documentation target directory (DOC_TARGET_DIR) not set." 1>&2; exit 15; fi
	@if [ ! -d "$(DOC_TARGET_DIR)" ]; then echo "  Error, documentation target directory '$(DOC_TARGET_DIR)' does not exist." 1>&2; exit 20; fi
	@/bin/cp -rf $(WEB_CONTENT) "$(DOC_TARGET_DIR)"



view-doc:
	@[ ! -e "$(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_PDF_TARGET)" ] || \
	$(PDF_VIEWER) $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_PDF_TARGET) \
	 1>/dev/null 2>&1


to-print: create-doc
	@echo "    Transferring document $(OVERALL_DOCUMENT_PDF_TARGET) \
	to $(PRINT_SERVER)"
	@scp $(TMP_RST_REPOSITORY)/$(OVERALL_DOCUMENT_PDF_TARGET) $(PRINT_LOCATION)


check-dot:
		@if [ ! -x "$(DOT)" ]; then echo "  Error, 'dot' tool, from graphviz, not found." 1>&2; exit 5; fi


clean: clean-doc-repository clean-generated clean-dot clean-doc-logs \
	clean-gnuplot clean-png clean-rst



clean-doc-repository:
	@#echo "     Cleaning documentation repository in $(TMP_RST_REPOSITORY)"
	-@/bin/rm -rf $(TMP_RST_REPOSITORY)


clean-generated:
	@#echo "     Cleaning generated files"
	-@/bin/rm -f $(RST_GENERATED_FILES)
	-@for f in $$(/bin/ls *.template 2>/dev/null); do /bin/rm -f $$(echo $$f | sed 's|\.template$$||1'); done




# Some .png must be kept (ex: dia-exported ones).
clean-dot:
	-@if [ $$(basename $$PWD) != "doc" ]; then \
	/bin/rm -f $(STATE_MACHINES_DIAGRAMS) *.map; fi


clean-doc-logs:
	-@/bin/rm -f *.log


# Let's hope we do not remove user files:
clean-gnuplot:
	-@if [ $$(basename $$PWD) != "doc" ]; then \
	/bin/rm -f *robe*.dat *robe*.p; fi


clean-png:
	-@/bin/rm -f $(GRAPH_PNG) $(PNG_FROM_PYTHON) $(PNG_FROM_PYTHON_NEGATED) \
	$(NEGATED_PNG) $(ROTATED_PNG)


clean-rst:
	-@/bin/rm -f $(RST_GENERATED_FILES)
	@if [ -d "$(TMP_RST_REPOSITORY)" ]; then $(MAKE) -s clean-doc-repository; fi
