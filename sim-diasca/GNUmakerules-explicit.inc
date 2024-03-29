# Here are gathered explicit (immediate, static) rules for Sim-Diasca.
#
# See GNUmakerules-explicit.inc for their automatic (generic, pattern-based)
# counterparts.


# Prerequisite: SIM_DIASCA_TOP must be set.


.PHONY: .PHONY-local


.PHONY-local: default-sim-diasca-rule make-config-links                       \
			  make-config-links-recurse create-doc-links                      \
			  docker-image prepare-docker-context clean-docker-context        \
			  prepare-docker-image                                            \
			  create-docker-image run-docker-image push-docker-image          \
			  info-docker-image                                               \
			  clean clean-probe clean-test                                    \
			  clean-test-logs clean-test-images clean-source-archives         \
			  clean-results clean-outputs



# The first non-generic, therefore default, rule:
default-sim-diasca-rule: all


# Ensures that, as soon a directory contains a test, there is an host
# configuration file, creating a symbolic link to the centralised version
# thereof.
#
# Run the clean-host-candidate-files target first, if wanting to rely on
# symbolic links pointing to the same centralised file.
#
make-config-links:
	@if [ ! -e "$(SIM_DIASCA_HOST_FILE)" ] ; then \
	if /bin/ls *_test.erl 1>/dev/null 2>&1; then \
	echo "  Adding link to centralised host file in $$(pwd)"; \
	/bin/ln -sf $(SIM_DIASCA_TOP)/conf/$(SIM_DIASCA_HOST_FILE); fi; fi


make-config-links-recurse:
	@for m in $(MODULES_DIRS); do \
	(cd $$m && $(MAKE) -s make-config-links make-config-links-recurse); \
	done



# Creates, once for all (before adding them in VCS) the main, general
# documentation-related symbolic links, used by most SimDiasca-related documents
# (especially the ones generated thanks to a local-html target):
#
create-doc-links:
	@echo "  Creating doc-related symlinks"
	@base_dir="$(SIM_DIASCA_TOP)/doc/common-elements"; for f in css/pygments-default.css css/sim-diasca.css edf-related/sim-diasca.png edf-related/logo-EDF-english.png edf-related/sim-diasca-icon.png licence/lgpl-v3-logo-bordered.png; do ln -sf $${base_dir}/$$f; done


prepare-docker-context:
	@echo "  Preparing a base Docker context, suitable for the creation of Sim-Diasca images"


clean-docker-context:
	@echo "  Cleaning Docker context"
	-@/bin/rm -f docker-context/Dockerfile docker-context/README-About-This-Docker-Image.txt 2>/dev/null
	-@/bin/rm -rf docker-context/Sim-Diasca/ 2>/dev/null


docker-image: prepare-docker-context prepare-docker-image create-docker-image
	@echo "If satisfied with this image, it can be pushed to registry thanks to: 'make push-docker-image'."


prepare-docker-image:
	@echo "  Preparing a Sim-Diasca $(IMAGE_BASE_NAME) Docker image"
	@$(DOCKER_PREPARATION_SCRIPT) $(DOCKER_USAGE_OPT) --erlang-version "$(ERLANG_TARGET_VERSION)"


create-docker-image: $(DOCKER_CONTEXT_NAME)/$(DOCKERFILE)
	@echo "  Tagging image for base registry, as $(IMAGE_BASE_REF_NAME) and $(IMAGE_BASE_LATEST_NAME)."
	@sudo docker build --tag $(IMAGE_BASE_REF_NAME) --tag $(IMAGE_BASE_LATEST_NAME) $(DOCKER_CONTEXT_NAME)
	@if [ -n "$(SIM_DIASCA_ALT_DOCKER_REGISTRY)" ]; then \
	echo "  Tagging image for alternate registry, as $(IMAGE_ALT_REF_NAME) and $(IMAGE_ALT_LATEST_NAME)."; \
	sudo docker build --tag $(IMAGE_ALT_REF_NAME) --tag $(IMAGE_ALT_LATEST_NAME) $(DOCKER_CONTEXT_NAME); fi
	@echo "This image can be checked interactively thanks to: 'sudo docker run -it $(IMAGE_BASE_REF_NAME) /bin/bash'."


run-docker-image:
	@echo "  Running image $(IMAGE_BASE_REF_NAME)"
	@sudo docker run -it "$(IMAGE_BASE_REF_NAME)"


push-docker-image:
	@echo "  Pushing all tags for image $(IMAGE_BASE_PUSH_TARGET) in base registry"
	@sudo docker image push --all-tags $(IMAGE_BASE_PUSH_TARGET)
	@if [ -n "$(SIM_DIASCA_ALT_DOCKER_REGISTRY)" ]; then \
	@echo "  Pushing all tags for image $(IMAGE_ALT_PUSH_TARGET) in alternate registry"; \
	sudo docker image push --all-tags $(IMAGE_ALT_PUSH_TARGET); fi


info-docker-image:
	@echo "DOCKER_REGISTRY = $(DOCKER_REGISTRY)"
	@echo "DOCKER_IMAGE_BASE_NAME = $(DOCKER_IMAGE_BASE_NAME)"
	@echo "IMAGE_BASE_NAME = $(IMAGE_BASE_NAME)"
	@echo "IMAGE_SUFFIX = $(IMAGE_SUFFIX)"
	@echo "IMAGE_BASE_TAGLESS = $(IMAGE_BASE_TAGLESS)"
	@echo "IMAGE_ALT_TAGLESS = $(IMAGE_ALT_TAGLESS)"
	@echo "IMAGE_BASE_REF_NAME = $(IMAGE_BASE_REF_NAME)"
	@echo "IMAGE_ALT_REF_NAME = $(IMAGE_ALT_REF_NAME)"
	@echo "IMAGE_BASE_LATEST_NAME = $(IMAGE_BASE_LATEST_NAME)"
	@echo "IMAGE_ALT_LATEST_NAME = $(IMAGE_ALT_LATEST_NAME)"
	@echo "IMAGE_BASE_PUSH_TARGET = $(IMAGE_BASE_PUSH_TARGET) "
	@echo "IMAGE_ALT_PUSH_TARGET = $(IMAGE_ALT_PUSH_TARGET) "




clean: clean-probe clean-test

clean-probe:
	-@if [ $$(basename $$(pwd)) != "doc" ]; then /bin/rm -f *_Probe*.png *_probe*.png *_probe*.p *_probe*.dat *_Probe*.png *_Probe*.p *_Probe*.dat *-generated.png; fi


clean-test: clean-test-logs clean-test-images clean-source-archives


clean-test-logs:
	-@/bin/rm -f *_test.log


# *test*.png was removing images that were to be kept:
clean-test-images:
	-@if [ $$(basename $$(pwd)) != "doc" ]; then \
	/bin/rm -f *Test*.png; fi


# Code archive files may be created by tests:
clean-source-archives:
	-@/bin/rm -f *.sdar



# This target is quite slow to evaluate and a bit dangerous (actually it is not
# called by default, see the 'clean-all-results' target in the root
# GNUmakefile):
#
clean-results:
	@echo "    Cleaning all results for user $$USER in $$(pwd)"
	@for d in *-on-*-at-*-by-$$USER-*; do /bin/rm -rf $$d; done


clean-outputs: clean-results clean-traces
	@for m in $(MODULES_DIRS); do \
	(cd $$m && $(MAKE) -s clean-outputs); \
	done


real-clean: clean-outputs


# Bootstrap section.


# Bootstrapped modules must be special-cased, as they should be built prior to
# any parse-transform using them (here, the Sim-Diasca one), and the resulting
# BEAM must be produced in the right directory (not at the engine root).
#
# As most paths (ex: the one leading to the Myriad parse transform) are
# relative, the corresponding directory change will make them irrelevant, so we
# add the correct ones (simpler than modifying the base paths).
#
# For actor_info, which is in src/core/generic (hence 3 levels away), and relies
#  only on myriad:
#
GENERIC_OFFSET := ../../..


# Both for usual build and OTP-based one:
BOOTSTRAP_OFFSET_PATHS_OPT_BEAMS := $(foreach dir,$(MYRIAD_BEAM_DIRS),-pz $(GENERIC_OFFSET)/$(dir)) -pz $(GENERIC_OFFSET)/../myriad/ebin


# Will not work (even if introducing a temporary variable such as 'ACTUAL_INC :=
# $(INC)'), as we are not at the bottom of all layers, for example Myriad's
# variables are still to be defined):
#
#BOOTSTRAP_OFFSET_PATHS_OPT_INCS := $(foreach dir,$(INC),-I$(GENERIC_OFFSET)/$(dir)) -I$(GENERIC_OFFSET)/../myriad/include


# So, instead of INC, we just hardcode what is needed:

BOOTSTRAP_INC_DIRS := include/core/generic

# Both for usual build and OTP-based one:
BOOTSTRAP_OFFSET_PATHS_OPT_INCS := $(foreach dir,$(BOOTSTRAP_INC_DIRS),-I$(GENERIC_OFFSET)/$(dir)) -I$(GENERIC_OFFSET)/../myriad/include


# See also: in GNUmakevars.inc, the BOOTSTRAP_MODULES variable.


# We still need a special build target here, so that the BEAM is generated in
# $(GENERIC_SRC) rather than at the root of Sim-Diasca:
#
# (refer to the Myriad counterpart bootstrap for more information)
#
$(GENERIC_SRC)/actor_info.beam: $(GENERIC_SRC)/actor_info.erl
	@echo "     Compiling bootstrap module $<"
	@#beam_file=$$(basename $@); cd $$(dirname $@) && pwd && echo $(ERLANG_COMPILER) $(BOOTSTRAP_OFFSET_PATHS_OPT_BEAMS) $(BOOTSTRAP_OFFSET_PATHS_OPT_INCS) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $$beam_file $$(echo $$beam_file | sed 's|.beam$$|.erl|1')
	@beam_file=$$(basename $@); cd $$(dirname $@) && $(ERLANG_COMPILER) $(BOOTSTRAP_OFFSET_PATHS_OPT_BEAMS) $(BOOTSTRAP_OFFSET_PATHS_OPT_INCS) $(ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES) -o $$beam_file $$(echo $$beam_file | sed 's|.beam$$|.erl|1')
