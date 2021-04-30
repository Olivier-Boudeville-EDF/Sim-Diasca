TRACES_TOP = .


.PHONY: help help-intro help-traces                                   \
		all register-version-in-header register-traces list-beam-dirs \
		add-prerequisite-plts link-plt                                \
		stats info-traces info-compile info-conditionals info-deps


MODULES_DIRS = src doc conf test examples

# To override the 'all' default target with a parallel version:
BASE_MAKEFILE = true



# Default target:
help: help-intro help-traces


help-intro:
	@echo " Following main make targets are available for package $(PACKAGE_NAME):"


help-traces:
	@cd $(WOOPER_TOP) && $(MAKE) -s help-wooper



register-version-in-header:
	@if [ -z "$(VERSION_FILE)" ] ; then \
	echo "Error, no version file defined." 1>&2 ; exit 52 ; else \
	$(MAKE) register-traces ; fi


register-traces:
	@echo "-define( traces_version, \"$(TRACES_VERSION)\" )." >> $(VERSION_FILE)


# Useful to extract internal layout for re-use in upper layers:
list-beam-dirs:
	@for d in $(TRACES_BEAM_DIRS) ; do echo $$(readlink -f $$d) ; done


add-prerequisite-plts: link-plt


# As upper layers may rely on the 'traces' naming:
link-plt:
	@if [ ! "$(PLT_FILE)" = "$(TRACES_PLT_FILE)" ]; then /bin/ln -s --force $(PLT_FILE) $(TRACES_PLT_FILE) ; fi


stats:
	@$(MAKE_CODE_STATS) $(TRACES_TOP)


info-traces:
	@echo "ENABLE_TRACE_OPT = $(ENABLE_TRACE_OPT)"


info-compile:
	@echo "ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES = $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES)"
	@echo "COMPILER_OPT_FOR_WOOPER_CLASSES = $(COMPILER_OPT_FOR_WOOPER_CLASSES)"


info-conditionals:
	@echo "TRACES_DEBUG_FLAGS = $(TRACES_DEBUG_FLAGS)"
	@echo "TRACES_CHECK_FLAGS = $(TRACES_CHECK_FLAGS)"


info-deps:
	@echo "MYRIAD_TOP = $(MYRIAD_TOP) (i.e. $$(realpath $(MYRIAD_TOP)))"
	@echo "WOOPER_TOP = $(WOOPER_TOP)) (i.e. $$(realpath $(WOOPER_TOP)))"


include $(TRACES_TOP)/GNUmakesettings.inc
