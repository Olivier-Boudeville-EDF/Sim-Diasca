MYRIAD_TOP = ../..


.PHONY: test-local prepare-otp-test test-distributions test-interactive-shell \
		archive info-archive clean clean-distribution-files real-clean


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


test-interactive-shell:
	@$(MAKE) -s shell_utils_run CMD_LINE_OPT="-extra --interactive-shell"


archive: random_utils_run
	@zip $(PLOT_ARCHIVE) compare_test_pdf_sampling* test_*.dat test_*.p test_*.png *.png


info: info-archive

info-archive:
	@echo "PLOT_ARCHIVE = $(PLOT_ARCHIVE)"


clean: clean-distribution-files


# Not wanting to remove compare_test_pdf_sampling.p.template:
clean-distribution-files:
	-@/bin/rm -f GNUmakefile.xz test_*.dat test_*.png *.png 2>/dev/null


real-clean:
	-@/bin/rm -rf *-probability-density-functions.zip


include $(MYRIAD_TOP)/GNUmakesettings.inc
