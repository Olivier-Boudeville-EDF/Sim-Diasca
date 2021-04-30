SIM_DIASCA_TOP = ../../../../..


.PHONY: test test-dumf-generation-from-excel-format \
		test-dumf-generation-from-open-document-format info-tools \
		clean clean-tmp

EXAMPLE_DIR := $(SIM_DIASCA_TOP)/src/core/services/dataflow/mockup-support/examples

TEST_EXCEL_SPREADSHEET := $(EXAMPLE_DIR)/MockupReferenceExample.xlsx

TEST_OPEN_DOCUMENT_SPREADSHEET := $(EXAMPLE_DIR)/MockupReferenceExample.ods


CONVERTER_OPT :=
#CONVERTER_OPT := --debug


test: test-dumf-generation-from-excel-format \
	  test-dumf-generation-from-open-document-format


test-dumf-generation-from-excel-format:
	@echo "  Testing the generation of a DUMF file from the $(TEST_EXCEL_SPREADSHEET) test reference"
	@$(SPREADSHEETS_TO_DUMF_SCRIPT) $(CONVERTER_OPT) $(TEST_EXCEL_SPREADSHEET)


test-dumf-generation-from-open-document-format:
	@echo "  Testing the generation of a DUMF file from the $(TEST_OPEN_DOCUMENT_SPREADSHEET) test reference"
	@$(SPREADSHEETS_TO_DUMF_SCRIPT) $(CONVERTER_OPT) $(TEST_OPEN_DOCUMENT_SPREADSHEET)


info-tools:
	@echo "SPREADSHEETS_TO_DUMF_SCRIPT = $(SPREADSHEETS_TO_DUMF_SCRIPT)"


clean: clean-tmp


clean-tmp:
	-@/bin/rm -rf *_tmp_dir


include $(SIM_DIASCA_TOP)/GNUmakesettings.inc
