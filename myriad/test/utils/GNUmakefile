MYRIAD_TOP = ../..


.PHONY: test-local prepare-otp-test test-distributions archive info-archive \
		clean-distribution-files


PLOT_ARCHIVE := $$(date '+%Y%m%d')-probability-density-functions.zip


# So that it remains the default target:
all:

test: test-local

test-local: prepare-otp-test
	@$(MAKE) -s most_basic_example_exec


prepare-otp-test:
	@cd $(MYRIAD_TOP) && $(MAKE) -s create-app-file


test-distributions: clean-distribution-files
	-@killall eog; $(MAKE) -s random_utils_run


archive: random_utils_run
	@zip $(PLOT_ARCHIVE) compare_test_pdf_sampling_for_* test_*.dat test_*.p test_*.png


info: info-archive

info-archive:
	@echo "PLOT_ARCHIVE = $(PLOT_ARCHIVE)"


clean: clean-distribution-files

clean-distribution-files:
	-@/bin/rm -f test_*.dat compare_test_pdf_sampling_for_*.p test_*.png 2>/dev/null


include $(MYRIAD_TOP)/GNUmakesettings.inc
