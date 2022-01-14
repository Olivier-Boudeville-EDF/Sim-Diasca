SIM_DIASCA_TOP = ../..


.PHONY: doc html pdf doc-local clean clean-local info-doc info-local-doc

MODULES_DIRS = correction


TRAINING_PREFIX = Sim-Diasca-training

OVERALL_DOCUMENT_SOURCE = $(TRAINING_PREFIX).rst

TRAINING_DIR = $(TRAINING_PREFIX)

TRAINING_ARCHIVE = $(TRAINING_PREFIX)-elements

TRAINING_ARCHIVE_FILENAME = $(TRAINING_ARCHIVE).tar.bz2


.PHONY: doc generate-training-archive clean clean-training


doc:
	@echo "  Run 'make full-doc' to generate the Sim-Diasca Training material (in English)."


html: full-html

pdf: full-pdf


generate-training-archive: clean full-doc
	@mkdir $(TRAINING_DIR)
	@/bin/cp tmp-rst/*.pdf $(TRAINING_DIR)
	@/bin/cp -r exercises $(TRAINING_DIR)
	@tar cvjf $(TRAINING_ARCHIVE_FILENAME) $(TRAINING_PREFIX)
	@echo; echo "$(TRAINING_ARCHIVE_FILENAME) is ready!"


clean: clean-training


clean-training:
	-@/bin/rm -rf $(TRAINING_DIR) $(TRAINING_ARCHIVE_FILENAME)



DOCUTILS_TOP = ..

include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
