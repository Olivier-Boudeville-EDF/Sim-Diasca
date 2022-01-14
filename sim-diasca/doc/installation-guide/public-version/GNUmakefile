SIM_DIASCA_TOP = ../../..


.PHONY: doc full-doc full-doc-stop local-doc html pdf secure-doc-elements \
		clean clean-doc-elements                                          \
		info-doc info-local-doc


OVERALL_DOCUMENT_SOURCE = $(SD_INST_PUBLIC_GUIDE_SRC)


# Default do-nothing target:
doc:
	@echo "  Run 'make local-doc' to generate the public installation " \
	"guide ($(SD_INST_PUBLIC_GUIDE_BASE)) documents."


# To shortcut inherited rule:
full-doc: full-doc-stop


full-doc-stop:
	@echo "For a proper generation, the (public) installation guide shall be created thanks to the 'local-doc' target." 1>&2
	@exit 16


local-doc: secure-doc-elements local-html local-pdf



# We used to rely on full-* targets instead of local-* ones, yet selecting
# recursively all RST files led to overwrite randomly identically-named files
# found in public and internal directories.
#
# So it is better to use local-* targets and to manage, on an ad hoc basis
# through symlinks, the various RST/images files needed:
#
html: secure-doc-elements local-html

pdf: secure-doc-elements local-pdf


secure-doc-elements:
	@echo "  Securing common RST and creating image symlinks"
	@cd .. && $(MAKE) -s secure-doc-elements
	@for f in $(SD_INST_COMMMON_RSTS); do ln -sf ../$$f; done
	@for f in $(SD_INST_COMMON_DOC_ELEMS) $(SD_XKCD)/xkcd-*.png $(SD_INST_EXTRAS); do ln -sf $$f; done


clean: clean-doc-elements

clean-doc-elements:
	@echo "  Cleaning doc elements: removing symlinks"
	@for f in $(SD_INST_COMMON_DOC_ELEMS) $(SD_INST_EXTRAS); do /bin/rm -f $$(basename $$f); done
	@for f in $(SD_INST_COMMMON_RSTS) xkcd-*.png; do /bin/rm -f $$f; done


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
