WOOPER_TOP = .


.PHONY: help help-intro help-wooper                                    \
		all register-version-in-header register-wooper list-beam-dirs  \
		add-prerequisite-plts link-plt                                 \
		release release-zip release-bz2 release-xz                     \
		prepare-release clean-release clean-archive stats              \
		info-paths info-compile info-parse-transform info-conditionals \
		info-deps


MODULES_DIRS = src doc conf test priv


# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true


WOOPER_RELEASES = $(WOOPER_RELEASE_ARCHIVE_BZ2) \
				  $(WOOPER_RELEASE_ARCHIVE_ZIP) \
				  $(WOOPER_RELEASE_ARCHIVE_XZ)



# First target for default:
help: help-intro help-wooper


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-wooper:
	@cd $(MYRIAD_TOP) && $(MAKE) -s help-myriad


register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 51 ; else \
	$(MAKE) register-wooper ; fi


register-wooper:
	@echo "-define( wooper_version, \"$(WOOPER_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(WOOPER_BEAM_DIRS) ; do echo $$(readlink -f $$d) ; done


add-prerequisite-plts: link-plt


# As upper layers may rely on the 'wooper' naming:
link-plt:
	@if [ ! "$(PLT_FILE)" = "$(WOOPER_PLT_FILE)" ]; then /bin/ln -s --force $(PLT_FILE) $(WOOPER_PLT_FILE) ; fi


# Note: the source archives are not produced in this directory, but in its
# parent, so that everything related to WOOPER (including these rules) remains
# self-contained.


release: release-zip release-bz2 release-xz
	@$(MAKE) clean-release


release-zip: prepare-release
	@echo "     Creating WOOPER release archive $(WOOPER_RELEASE_ARCHIVE_ZIP)"
	@cd .. && zip -r $(WOOPER_RELEASE_ARCHIVE_ZIP) $(WOOPER_RELEASE_BASE) \
	&& echo "     Archive $(WOOPER_RELEASE_ARCHIVE_ZIP) ready in "`pwd`


release-bz2: prepare-release
	@echo "     Creating WOOPER release archive $(WOOPER_RELEASE_ARCHIVE_BZ2)"
	@cd .. && tar chvjf $(WOOPER_RELEASE_ARCHIVE_BZ2) $(WOOPER_RELEASE_BASE) \
	&& echo "     Archive $(WOOPER_RELEASE_ARCHIVE_BZ2) ready in "`pwd`


release-xz: prepare-release
	@echo "     Creating WOOPER release archive $(WOOPER_RELEASE_ARCHIVE_XZ)"
	@cd .. && tar chvjf $(WOOPER_RELEASE_ARCHIVE_XZ) $(WOOPER_RELEASE_BASE) \
	&& echo "     Archive $(WOOPER_RELEASE_ARCHIVE_XZ) ready in "`pwd`


# The '-L' option with cp is used so that symbolic links are replaced by their
# actual target file, otherwise tar would include dead links in releases.
#
prepare-release: clean clean-release
	@echo "     Preparing release archive for WOOPER $(WOOPER_VERSION)"
	@cd .. && mkdir -p $(WOOPER_RELEASE_BASE) && /bin/cp -L -r myriad wooper $(WOOPER_RELEASE_BASE)
	@cd ../$(WOOPER_RELEASE_BASE) && mv wooper/top-GNUmakefile-for-releases GNUmakefile
	-@cd .. && find $(WOOPER_RELEASE_BASE) -type d -a -name '.git' -exec /bin/rm -rf '{}' ';' 2>/dev/null
	-@cd .. && find $(WOOPER_RELEASE_BASE) -type f -a -name '*.beam' -exec /bin/rm -f '{}' ';' 2>/dev/null


clean: clean-release clean-archive


clean-release:
	@echo "   Cleaning release archive for WOOPER"
	-@cd .. && /bin/rm -rf $(WOOPER_RELEASE_BASE)


clean-archive:
	-@cd .. && /bin/rm -f $(WOOPER_RELEASES)


stats:
	@$(MAKE_CODE_STATS) $(WOOPER_TOP)


info-paths:
	@echo "BEAM_PATH_OPT = $(BEAM_PATH_OPT)"


info-compile:
	@echo "ERLANG_COMPILER_BASE_OPT = $(ERLANG_COMPILER_BASE_OPT)"
	@echo "BEAM_DIRS = $(BEAM_DIRS)"
	@echo "INC = $(INC)"
	@echo "ERLANG_COMPILER_EXEC_TARGET_OPT = $(ERLANG_COMPILER_EXEC_TARGET_OPT)"
	@echo "ERLANG_COMPILER_DEBUG_OPT = $(ERLANG_COMPILER_DEBUG_OPT)"
	@echo "ERLANG_COMPILER_NATIVE_COMPILATION_OPT = $(ERLANG_COMPILER_NATIVE_COMPILATION_OPT)"
	@echo "ERLANG_COMPILER_WARNING_OPT = $(ERLANG_COMPILER_WARNING_OPT)"
	@echo "ERLANG_COMPILER_OPT_BASE = $(ERLANG_COMPILER_OPT_BASE)"
	@echo "OVERALL_PZ_OPT = $(OVERALL_PZ_OPT)"
	@echo "ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES = $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES)"
	@echo "COMPILER_OPT_FOR_WOOPER_CLASSES = $(COMPILER_OPT_FOR_WOOPER_CLASSES)"
	@echo "EXECUTION_TARGET = $(EXECUTION_TARGET)"
	@echo "ENABLE_DEBUG = $(ENABLE_DEBUG)"
	@echo "ENABLE_WOOPER_DEBUG_OPT = $(ENABLE_WOOPER_DEBUG_OPT)"


info-parse-transform:
	@echo "BOOTSTRAP_MODULES = $(BOOTSTRAP_MODULES)"
	@echo "ERLANG_COMPILER_OPT_FOR_PT = $(ERLANG_COMPILER_OPT_FOR_PT)"
	@echo "META_BEAM_FILES = $(META_BEAM_FILES)"
	@echo "ERLANG_COMPILER_PARSE_TRANSFORM_OPT = $(ERLANG_COMPILER_PARSE_TRANSFORM_OPT)"


info-conditionals:
	@echo "WOOPER_DEBUG_FLAGS = $(WOOPER_DEBUG_FLAGS)"
	@echo "WOOPER_CHECK_FLAGS = $(WOOPER_CHECK_FLAGS)"


info-deps:
	@echo "MYRIAD_TOP = $(MYRIAD_TOP) (i.e. $$(realpath $(MYRIAD_TOP)))"


include $(WOOPER_TOP)/GNUmakesettings.inc
