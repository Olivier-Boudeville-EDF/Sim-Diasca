SIM_DIASCA_TOP = ../../..



CORRECTION_PREFIX = Sim-Diasca-training-corrected-exercises

OVERALL_DOCUMENT_SOURCE = $(CORRECTION_PREFIX).rst

CORRECTION_DIR = $(CORRECTION_PREFIX)

CORRECTION_ARCHIVE = $(CORRECTION_PREFIX)

CORRECTION_ARCHIVE_FILENAME = $(CORRECTION_ARCHIVE).tar.bz2


.PHONY: doc generate-correction-archive clean clean-correction

doc:
	@echo "  Run 'make full-doc' to generate the corrections corresponding to the Sim-Diasca Training material (in English)."


generate-correction-archive: clean full-doc
	@mkdir $(CORRECTION_DIR)
	@/bin/cp tmp-rst/*.pdf $(CORRECTION_DIR)
	@/bin/cp -r ex_* $(CORRECTION_DIR)
	@tar cvjf $(CORRECTION_ARCHIVE_FILENAME) $(CORRECTION_PREFIX)
	@echo ; echo "$(CORRECTION_ARCHIVE_FILENAME) is ready!"


clean: clean-correction


clean-correction:
	-@/bin/rm -rf $(CORRECTION_DIR) $(CORRECTION_ARCHIVE_FILENAME)



DOCUTILS_TOP = ../..

include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
