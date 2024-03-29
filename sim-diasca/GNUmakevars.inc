# Prerequisite: SIM_DIASCA_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)
	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)
	ENABLE_DEBUG = false

else

	$(error Invalid Sim-Diasca execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif


# Enforce default values:

# Define the simdiasca_allow_reproducible_nested_initial_creations token if
# wanting to enable fully-reproducible initial nested creations, i.e. initial
# actors that create other actors from their constructors (the consequence is
# then to use a single creator process, instead of multiple, parallel ones -
# thus resulting in longer initialisations):
#
# (refer to the design notes in instance_loading.erl for more details)
#
ifndef SIM_DIASCA_SETTINGS_FLAGS
	SIM_DIASCA_SETTINGS_FLAGS += -Dsim_diasca_allow_reproducible_nested_initial_creations
endif


# Defining (optional) engine-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management; a
# corresponding tracing of their enabling shall be made in sim_diasca:init/3 for
# a better control thereof):

# Topics covered by flags:
#  - user_model_behaviours the behaviour of the user-specified models
#  - user_api_calls: the calls by the user code of the engine-provided services
#  - time_management: the time management, as implemented by the engine itself
#  - initial_creations: the creation of initial actors, from the case
#  - instance_loading: the creation of initial actors from initialisation
#    streams (typically files)
#  - runtime_creations: the creation of actors in the course of the simulation
#  - life_cycles: the life cycles, typically of actors
#  - graph_streaming: the management of graph streaming, notably the related
#    probes

# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	#SIM_DIASCA_DEBUG_FLAGS += -Dsim_diasca_debug_model_behaviours
	#SIM_DIASCA_DEBUG_FLAGS += -Dsim_diasca_debug_user_api_calls
	#SIM_DIASCA_DEBUG_FLAGS += -Dsim_diasca_debug_time_management
	#SIM_DIASCA_DEBUG_FLAGS += -Dsim_diasca_debug_initial_creations
	#SIM_DIASCA_DEBUG_FLAGS += -Dsim_diasca_debug_instance_loading
	#SIM_DIASCA_DEBUG_FLAGS += -Dsim_diasca_debug_runtime_creations
	#SIM_DIASCA_DEBUG_FLAGS += -Dsim_diasca_debug_life_cycles
	#SIM_DIASCA_DEBUG_FLAGS += -Dsim_diasca_debug_graph_streaming
endif


# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	SIM_DIASCA_CHECK_FLAGS += -Dsim_diasca_check_model_behaviours
	SIM_DIASCA_CHECK_FLAGS += -Dsim_diasca_check_user_api_calls
	SIM_DIASCA_CHECK_FLAGS += -Dsim_diasca_check_time_management
	SIM_DIASCA_CHECK_FLAGS += -Dsim_diasca_check_initial_creations
	SIM_DIASCA_CHECK_FLAGS += -Dsim_diasca_check_instance_loading
	SIM_DIASCA_CHECK_FLAGS += -Dsim_diasca_check_runtime_creations
	SIM_DIASCA_CHECK_FLAGS += -Dsim_diasca_check_life_cycles
	SIM_DIASCA_DEBUG_FLAGS += -Dsim_diasca_check_graph_streaming
endif


ERLANG_COMPILER_OPT_BASE += -Dsim_diasca_version=\"$(SIM_DIASCA_VERSION)\" \
							$(SIM_DIASCA_SETTINGS_FLAGS)                   \
							$(SIM_DIASCA_DEBUG_FLAGS)                      \
							$(SIM_DIASCA_CHECK_FLAGS)




# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := Sim-Diasca
endif


SIM_DIASCA_BASE_VERSION := 2.4.7

# The uniquely-defined version of this layer, for all uses (including rebar, hex
# and the code itself, see sim_diascas:get_sim_diasca_version{_string,}/0):
#
#SIM_DIASCA_VERSION := $(SIM_DIASCA_BASE_VERSION)-beta-$(date '+%Y%m%d-%Hh%M')
#SIM_DIASCA_VERSION := $(SIM_DIASCA_BASE_VERSION)-for-foobar
#SIM_DIASCA_VERSION := $(SIM_DIASCA_BASE_VERSION)-rc1
SIM_DIASCA_VERSION := $(SIM_DIASCA_BASE_VERSION)


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(SIM_DIASCA_VERSION)
endif

ifndef PROJECT_RELEASE_DATE
	# To be set only at release time:
	#PROJECT_RELEASE_DATE := "(not set yet)"
	PROJECT_RELEASE_DATE := "Tuesday, February 13, 2024"
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME := sim-diasca
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP := $(SIM_DIASCA_TOP)
endif


ifndef INSTALLATION_PREFIX
	INSTALLATION_PREFIX := $$HOME/Software/Sim-Diasca/Sim-Diasca-$(SIM_DIASCA_VERSION)
endif


# Better defined here, as depends on the Sim-Diasca version number:
SIM_DIASCA_RELEASE_BASENAME := Sim-Diasca-$(SIM_DIASCA_VERSION)
SIM_DIASCA_RELEASE_BASE := $(ROOT_TOP)/../$(SIM_DIASCA_RELEASE_BASENAME)

SIM_DIASCA_RELEASE_ARCHIVE_ZIP := $(SIM_DIASCA_RELEASE_BASENAME).zip
SIM_DIASCA_RELEASE_ARCHIVE_BZ2 := $(SIM_DIASCA_RELEASE_BASENAME).tar.bz2
SIM_DIASCA_RELEASE_ARCHIVE_XZ  := $(SIM_DIASCA_RELEASE_BASENAME).tar.xz

SIM_DIASCA_RELEASES := $(SIM_DIASCA_RELEASE_ARCHIVE_ZIP) \
					   $(SIM_DIASCA_RELEASE_ARCHIVE_BZ2) \
					   $(SIM_DIASCA_RELEASE_ARCHIVE_XZ)


ifndef VM_NAME
	VM_NAME := sim_diasca_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := sim_diasca_test
endif


# Starting from this layer, nodes are not started as distributed ones, as, for
# the purpose of run uniqueness (see SII), Sim-Diasca has to change the name of
# the user node at runtime - hence this non-distributed flag (that is forced now):
#
NODE_NAMING := "--nn"


WORK_ARCHIVE_FILE  := \
  $(ARCHIVE_ROOT)/$(date +'%Y%m%d')-$(PROJECT_NAME)-work-archive.tar.xz



# Directory section.

VERSION_DIR := $(SIM_DIASCA_TOP)/include

FRAMEWORK_DIR := $(SIM_DIASCA_TOP)/src

CORE_BASE_DIR := $(FRAMEWORK_DIR)/core

SERVICES_BASE_DIR := $(CORE_BASE_DIR)/services

BINDINGS_BASE_DIR           := $(SERVICES_BASE_DIR)/bindings
CASE_SUPPORT_BASE_DIR       := $(SERVICES_BASE_DIR)/case-support
CONTROL_MANAGEMENT_BASE_DIR := $(SERVICES_BASE_DIR)/control-management
DATAFLOW_BASE_DIR           := $(SERVICES_BASE_DIR)/dataflow
DATA_MANAGEMENT_BASE_DIR    := $(SERVICES_BASE_DIR)/data-management
DEPLOYMENT_BASE_DIR         := $(SERVICES_BASE_DIR)/deployment
INSTANCE_CREATION_BASE_DIR  := $(SERVICES_BASE_DIR)/instance-creation
PLUGINS_BASE_DIR            := $(SERVICES_BASE_DIR)/plugins
RANDOM_BASE_DIR             := $(SERVICES_BASE_DIR)/random
RESILIENCE_BASE_DIR         := $(SERVICES_BASE_DIR)/resilience
SCHEDULING_BASE_DIR         := $(SERVICES_BASE_DIR)/scheduling

JAVA_BINDING_BASE_DIR   := $(BINDINGS_BASE_DIR)/java
PYTHON_BINDING_BASE_DIR := $(BINDINGS_BASE_DIR)/python

DATA_EXCHANGE_BASE_DIR := $(DATA_MANAGEMENT_BASE_DIR)/data-exchange
DATA_STORAGE_BASE_DIR  := $(DATA_MANAGEMENT_BASE_DIR)/data-storage
MONITORING_BASE_DIR    := $(DATA_MANAGEMENT_BASE_DIR)/monitoring
RESULT_BASE_DIR        := $(DATA_MANAGEMENT_BASE_DIR)/result-management

WEB_BASE_DIR := $(CONTROL_MANAGEMENT_BASE_DIR)/webserver


MOCKUP_BASE_DIR            := $(DATAFLOW_BASE_DIR)/mockup-support
DATAFLOW_BINDINGS_BASE_DIR := $(DATAFLOW_BASE_DIR)/bindings

DATAFLOW_JAVA_BINDING_BASE_DIR   := $(DATAFLOW_BINDINGS_BASE_DIR)/java
DATAFLOW_PYTHON_BINDING_BASE_DIR := $(DATAFLOW_BINDINGS_BASE_DIR)/python

PERFORMANCE_BASE_DIR  := $(MONITORING_BASE_DIR)/performance

BASIC_PROBE_BASE_DIR        := $(MONITORING_BASE_DIR)/basic-probe
WEB_PROBE_BASE_DIR          := $(MONITORING_BASE_DIR)/web-probe
GRAPH_STREAM_PROBE_BASE_DIR := $(MONITORING_BASE_DIR)/graph-stream-probe

GENERIC_BASE_DIR := $(CORE_BASE_DIR)/generic
MESH_BASE_DIR    := $(CORE_BASE_DIR)/mesh
SPACE_BASE_DIR   := $(CORE_BASE_DIR)/space


MODELS_DIR = $(FRAMEWORK_DIR)/models

EQUIPMENT_BASE_DIR   := $(MODELS_DIR)/equipment
RELIABILITY_BASE_DIR := $(MODELS_DIR)/reliability

FAILURE_BASE_DIR     := $(RELIABILITY_BASE_DIR)/failure
REPARATION_BASE_DIR  := $(RELIABILITY_BASE_DIR)/reparation

POST_PROCESSING_BASE_DIR := $(FRAMEWORK_DIR)/post-processing

SPATIAL_BASE_DIR := $(FRAMEWORK_DIR)/spatial-support



# Source section.

CORE_SRC := $(CORE_BASE_DIR)

DATA_EXCHANGER_SRC      := $(DATA_EXCHANGE_BASE_DIR)
DATA_LOGGER_SRC         := $(DATA_STORAGE_BASE_DIR)

WEB_SRC := $(WEB_BASE_DIR)

PERFORMANCE_TRACKER_SRC := $(PERFORMANCE_BASE_DIR)
BASIC_PROBE_SRC         := $(BASIC_PROBE_BASE_DIR)
WEB_PROBE_SRC           := $(WEB_PROBE_BASE_DIR)
GRAPH_STREAM_PROBE_SRC  := $(GRAPH_STREAM_PROBE_BASE_DIR)
RESULT_SRC              := $(RESULT_BASE_DIR)

BINDINGS_SRC          := $(BINDINGS_BASE_DIR)
CASE_SUPPORT_SRC      := $(CASE_SUPPORT_BASE_DIR)
DATAFLOW_SRC          := $(DATAFLOW_BASE_DIR)
DEPLOYMENT_SRC        := $(DEPLOYMENT_BASE_DIR)
PLUGINS_SRC           := $(PLUGINS_BASE_DIR)
RANDOM_SRC            := $(RANDOM_BASE_DIR)
RESILIENCE_SRC        := $(RESILIENCE_BASE_DIR)
SCHEDULING_SRC        := $(SCHEDULING_BASE_DIR)
INSTANCE_CREATION_SRC := $(INSTANCE_CREATION_BASE_DIR)


JAVA_BINDING_SRC   := $(BINDINGS_BASE_DIR)/java
PYTHON_BINDING_SRC := $(BINDINGS_BASE_DIR)/python

DATAFLOW_BINDINGS_SRC := $(DATAFLOW_BINDINGS_BASE_DIR)

DATAFLOW_JAVA_BINDING_SRC   := $(DATAFLOW_JAVA_BINDING_BASE_DIR)
DATAFLOW_PYTHON_BINDING_SRC := $(DATAFLOW_PYTHON_BINDING_BASE_DIR)

MOCKUP_SRC := $(MOCKUP_BASE_DIR)


GENERIC_SRC := $(GENERIC_BASE_DIR)
MESH_SRC    := $(MESH_BASE_DIR)
SPACE_SRC   := $(SPACE_BASE_DIR)


EQUIPMENT_SRC   := $(EQUIPMENT_BASE_DIR)
RELIABILITY_SRC := $(RELIABILITY_BASE_DIR)


FAILURE_SRC     := $(FAILURE_BASE_DIR)
REPARATION_SRC  := $(REPARATION_BASE_DIR)


SPATIAL_SRC := $(SPATIAL_BASE_DIR)



# Include path section.


# In an OTP/rebar3-style application layout, at compilation time, libraries
# making use of Traces will expect its includes to be located in:
#
# (Myriad and WOOPER already taken care of, at the Traces level)
#
TRACES_OTP_INC := -I$(SIM_DIASCA_TOP)/../traces/include/


VERSION_INC = -I$(VERSION_DIR)

INC += $(TRACES_OTP_INC) $(VERSION_INC)


# BEAM path section.

CORE_BEAM := $(CORE_BASE_DIR)

SERVICES_BEAM            := $(SERVICES_BASE_DIR)

BINDINGS_BEAM            := $(BINDINGS_BASE_DIR)
CASE_SUPPORT_BEAM        := $(CASE_SUPPORT_BASE_DIR)
DATAFLOW_BEAM            := $(DATAFLOW_BASE_DIR)
DATA_EXCHANGE_BEAM       := $(DATA_EXCHANGE_BASE_DIR)
DATA_STORAGE_BEAM        := $(DATA_STORAGE_BASE_DIR)
WEB_BEAM                 := $(WEB_BASE_DIR)
DEPLOYMENT_BEAM          := $(DEPLOYMENT_BASE_DIR)
INSTANCE_CREATION_BEAM   := $(INSTANCE_CREATION_BASE_DIR)
PERFORMANCE_TRACKER_BEAM := $(PERFORMANCE_BASE_DIR)
BASIC_PROBE_BEAM         := $(BASIC_PROBE_BASE_DIR)
WEB_PROBE_BEAM           := $(WEB_PROBE_BASE_DIR)
GRAPH_STREAM_PROBE_BEAM  := $(GRAPH_STREAM_PROBE_BASE_DIR)
PLUGINS_BEAM             := $(PLUGINS_BASE_DIR)
RANDOM_BEAM              := $(RANDOM_BASE_DIR)
RESILIENCE_BEAM          := $(RESILIENCE_BASE_DIR)
RESULT_BEAM              := $(RESULT_BASE_DIR)
SCHEDULING_BEAM          := $(SCHEDULING_BASE_DIR)

JAVA_BINDING_BEAM   := $(JAVA_BINDING_SRC)
PYTHON_BINDING_BEAM := $(PYTHON_BINDING_SRC)

DATAFLOW_BINDINGS_BEAM := $(DATAFLOW_BINDINGS_BASE_DIR)

DATAFLOW_JAVA_BINDING_BEAM   := $(DATAFLOW_JAVA_BINDING_SRC)
DATAFLOW_PYTHON_BINDING_BEAM := $(DATAFLOW_PYTHON_BINDING_SRC)

MOCKUP_BEAM := $(MOCKUP_BASE_DIR)


GENERIC_BEAM := $(GENERIC_BASE_DIR)
MESH_BEAM    := $(MESH_BASE_DIR)
SPACE_BEAM   := $(SPACE_BASE_DIR)

EQUIPMENT_BEAM  := $(EQUIPMENT_BASE_DIR)
FAILURE_BEAM    := $(FAILURE_BASE_DIR)
REPARATION_BEAM := $(REPARATION_BASE_DIR)

POST_PROCESSING_BEAM := $(POST_PROCESSING_BASE_DIR)

SPATIAL_BEAM := $(SPATIAL_BASE_DIR)


# Used to be 'BEAM_DIRS := ... $(BEAM_DIRS)', but it led to multiple duplicates
# (multiple inclusions?)
#
# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
SIM_DIASCA_BEAM_DIRS += $(CORE_BEAM) $(SERVICES_BEAM)                         \
	$(BINDINGS_BEAM) $(CASE_SUPPORT_BEAM) $(DATAFLOW_BEAM)                    \
	$(DATA_EXCHANGE_BEAM) $(DATA_STORAGE_BEAM) $(WEB_BEAM) $(DEPLOYMENT_BEAM) \
	$(INSTANCE_CREATION_BEAM) $(PERFORMANCE_TRACKER_BEAM) $(PLUGINS_BEAM)     \
	$(BASIC_PROBE_BEAM) $(WEB_PROBE_BEAM) $(GRAPH_STREAM_PROBE_BEAM)          \
	$(RANDOM_BEAM) $(RESILIENCE_BEAM) $(RESULT_BEAM)                          \
	$(SCHEDULING_BEAM)                                                        \
	$(BINDINGS_BEAM) $(JAVA_BINDING_BEAM) $(PYTHON_BINDING_BEAM)              \
	$(DATAFLOW_BINDINGS_BEAM) $(DATAFLOW_JAVA_BINDING_BEAM)                   \
	$(DATAFLOW_PYTHON_BINDING_BEAM) $(MOCKUP_BEAM)                            \
	$(GENERIC_BEAM) $(MESH_BEAM) $(SPACE_BEAM) $(EQUIPMENT_BEAM)              \
	$(FAILURE_BEAM) $(REPARATION_BEAM) $(POST_PROCESSING_BEAM)                \
	$(SPATIAL_BEAM)                                                           \
	$(SIM_DIASCA_TOP)/..


# We rely on the prerequisites as well:
BEAM_DIRS += $(SIM_DIASCA_BEAM_DIRS)



# Compiler section.


# Modules needed for the bootstrapping for others (hence to be built first, and
# not parse-transformed; typically *used* by parse-transforms, or being a
# parse-transform themselves)
#
# (see the 'Bootstrap section' in GNUmakerules-explicit.inc for their
# special-cased build)
#
ifndef BOOTSTRAP_MODULES

	# We want to bootstrap all relevant Sim-Diasca modules (WOOPER and Myriad
	# expected to be already fully built)


	# We list here all the SimDiasca-level prerequisites of the Sim-Diasca
	# parse-transform:
	#
	# - actor_info: to manage actor-level information

	BOOTSTRAP_MODULES = $(GENERIC_SRC)/actor_info.beam

endif


# To be available from a Makefile despite not being in the user's PATH:
KILL_SIM_INTERACT_TOOLS = $(SIM_DIASCA_TOP)/priv/bin/kill-simulation-interactive-tools.sh


# To compile a module (ex: a plugin) using a behaviour (ex: sim_diasca_plugin),
# the BEAM of that behaviour must be found:
#
ERLANG_COMPILER_OPT_BASE += -pa $(PLUGINS_BEAM)


# Variable defined to be re-usable by upper layers, so that compiling their own
# parse transforms can take advantage of the WOOPER one:
#
ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_SIM_DIASCA =                \
					$(OVERALL_PA_OPTS)                              \
					-pz $(WOOPER_SRC)                               \
					'+{parse_transform,sim_diasca_parse_transform}'


# Different console messages, and using the Sim-Diasca parse transform:
#OVERRIDE_WOOPER_BUILD_RULES := true


COMPILER_OPT_FOR_SIM_DIASCA_CLASSES =                             \
			$(ERLANG_COMPILER_OPT_BASE)                           \
			$(ENABLE_WOOPER_DEBUG_OPT)                            \
			$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_SIM_DIASCA)


# Best option to avoid various warnings (same as for
# trace_disabled/{1,2,3,4,5}):
#
ERLANG_COMPILER_OPT_BASE += "+{nowarn_unused_function,[ {get_spatial_message,2} ]}"



## Parse-transform related section.


# List here all the directories expected to contain parse transforms:
#
# (to be overridden by each upper layer)
#
ifndef PARSE_TRANSFORM_DIRS
	PARSE_TRANSFORM_DIRS = $(GENERIC_SRC)
endif


# Specifying the root directory of Sim-Diasca allows to be able to find
# afterwards files (like the node cleaner script) regardless from where the
# interpreter is run.
#
EXEC_EXTERNAL_OPTIONS := --sim-diasca-root $(SIM_DIASCA_TOP) \
			 --sim-diasca-version $(SIM_DIASCA_VERSION) $(EXEC_EXTERNAL_OPTIONS)


# The script to convert spreadsheets into DUMF files:
SPREADSHEETS_TO_DUMF_SCRIPT := $(MOCKUP_BASE_DIR)/mockup_spreadsheet_to_dumf.escript


# Must be available from the user's path:
LOGMX := $$(which logmx.sh 2>/dev/null)


# Whence the reference CSS files come:
ifndef CSS_ROOT
	CSS_ROOT := $(SIM_DIASCA_TOP)/doc/common-elements/css
endif


# Expected to have been copied/symlinked and thus to be local:
ifndef PROJECT_CSS
	PROJECT_CSS := pygments-default.css,sim-diasca.css
endif


ifndef PROJECT_ICON
	PROJECT_ICON := $(SIM_DIASCA_TOP)/doc/common-elements/edf-related/sim-diasca-icon.png
endif


# The configuration file to list the potential distributed hosts:
SIM_DIASCA_HOST_FILE := "sim-diasca-host-candidates.txt"



# Java section.

ifeq ($(USE_JAVA_BINDING),true)

	JAVA_API_BASE := $(SIM_DIASCA_TOP)/core/services/dataflow/bindings/java/api

	# Root for the 'myriad' and 'sim_diasca' packages:
	CLASSPATH_DIRS += $(JAVA_API_BASE)

endif




# Documentation section.


SD_PREFIX := Sim-Diasca
SD_LANG := english

SD_DOC_TOP := $(SIM_DIASCA_TOP)/doc
SD_COMMON_ELEM_DIR := $(SD_DOC_TOP)/common-elements
SD_XKCD := $(SD_COMMON_ELEM_DIR)/xkcd

SD_DOC_OVERVIEW_NAME := documentation-overview

SD_DOC_OVERVIEW_BASE_DIR := $(SD_DOC_OVERVIEW_NAME)

SD_DOC_OVERVIEW_INTERNAL_DIR := $(SD_DOC_OVERVIEW_BASE_DIR)/internal-version
SD_DOC_OVERVIEW_PUBLIC_DIR := $(SD_DOC_OVERVIEW_BASE_DIR)/public-version

SD_DOC_OVERVIEW_INTERNAL_BASE := $(SD_PREFIX)-internal-$(SD_DOC_OVERVIEW_NAME)-$(SD_LANG)
SD_DOC_OVERVIEW_PUBLIC_BASE := $(SD_PREFIX)-public-$(SD_DOC_OVERVIEW_NAME)-$(SD_LANG)

SD_DOC_OVERVIEW_INTERNAL_TMPL := $(SD_DOC_OVERVIEW_INTERNAL_BASE).rst.template
SD_DOC_OVERVIEW_PUBLIC_TMPL := $(SD_DOC_OVERVIEW_PUBLIC_BASE).rst.template

SD_DOC_OVERVIEW_INTERNAL_SRC := $(SD_DOC_OVERVIEW_INTERNAL_BASE).rst
SD_DOC_OVERVIEW_PUBLIC_SRC := $(SD_DOC_OVERVIEW_PUBLIC_BASE).rst

SD_DOC_OVERVIEW_INTERNAL_HTML := $(SD_DOC_OVERVIEW_INTERNAL_BASE).html
SD_DOC_OVERVIEW_PUBLIC_HTML := $(SD_DOC_OVERVIEW_PUBLIC_BASE).html

SD_DOC_OVERVIEW_INTERNAL_PDF := $(SD_DOC_OVERVIEW_INTERNAL_BASE).pdf
SD_DOC_OVERVIEW_PUBLIC_PDF := $(SD_DOC_OVERVIEW_PUBLIC_BASE).pdf



SD_TECH_MANUAL_NAME := technical-manual
SD_TECH_MANUAL_DIR := $(SD_TECH_MANUAL_NAME)
SD_TECH_MANUAL_BASE := $(SD_PREFIX)-$(SD_TECH_MANUAL_NAME)-$(SD_LANG)
SD_TECH_MANUAL_SRC := $(SD_TECH_MANUAL_BASE).rst
SD_TECH_MANUAL_HTML := $(SD_TECH_MANUAL_BASE).html
SD_TECH_MANUAL_PDF := $(SD_TECH_MANUAL_BASE).pdf



SD_INST_GUIDE_NAME := installation-guide

SD_INST_GUIDE_BASE_DIR := $(SD_INST_GUIDE_NAME)

SD_INST_GUIDE_INTERNAL_DIR := $(SD_INST_GUIDE_BASE_DIR)/internal-version
SD_INST_GUIDE_PUBLIC_DIR := $(SD_INST_GUIDE_BASE_DIR)/public-version

SD_INST_INTERNAL_GUIDE_BASE := $(SD_PREFIX)-internal-$(SD_INST_GUIDE_NAME)-$(SD_LANG)
SD_INST_PUBLIC_GUIDE_BASE := $(SD_PREFIX)-public-$(SD_INST_GUIDE_NAME)-$(SD_LANG)

SD_INST_INTERNAL_GUIDE_SRC := $(SD_INST_INTERNAL_GUIDE_BASE).rst
SD_INST_PUBLIC_GUIDE_SRC := $(SD_INST_PUBLIC_GUIDE_BASE).rst

SD_INST_INTERNAL_GUIDE_HTML := $(SD_INST_INTERNAL_GUIDE_BASE).html
SD_INST_PUBLIC_GUIDE_HTML := $(SD_INST_PUBLIC_GUIDE_BASE).html

SD_INST_INTERNAL_GUIDE_PDF := $(SD_INST_INTERNAL_GUIDE_BASE).pdf
SD_INST_PUBLIC_GUIDE_PDF := $(SD_INST_PUBLIC_GUIDE_BASE).pdf


# So that they can be cherry-picked for local-{html,pdf} rules:
SD_INST_COMMON_DOC_ELEMS := \
	$(SD_COMMON_ELEM_DIR)/licence/lgpl-v3-logo-bordered.png      \
	$(SD_COMMON_ELEM_DIR)/edf-related/logo-EDF-english.png       \
	$(SD_COMMON_ELEM_DIR)/edf-related/sim-diasca-icon.png        \
	$(SD_COMMON_ELEM_DIR)/edf-related/sim-diasca.png             \
	$(SD_COMMON_ELEM_DIR)/css/pygments-default.css               \
	$(SD_COMMON_ELEM_DIR)/css/sim-diasca.css

SD_INST_EXTRAS := $(SD_DOC_TOP)/$(SD_TECH_MANUAL_DIR)/building-blocks/logmx-interface.png

SD_INST_COMMMON_RSTS := Sim-Diasca-installation-guide-header-english.rst \
						Sim-Diasca-installation-guide-base-english.rst


SD_DEV_GUIDE_NAME := developer-guide
SD_DEV_GUIDE_DIR := $(SD_DEV_GUIDE_NAME)
SD_DEV_GUIDE_BASE := $(SD_PREFIX)-$(SD_DEV_GUIDE_NAME)-$(SD_LANG)
SD_DEV_GUIDE_SRC := $(SD_DEV_GUIDE_BASE).rst
SD_DEV_GUIDE_HTML := $(SD_DEV_GUIDE_BASE).html
SD_DEV_GUIDE_PDF := $(SD_DEV_GUIDE_BASE).pdf



SD_MOD_GUIDE_NAME := modeller-guide
SD_MOD_GUIDE_DIR := $(SD_MOD_GUIDE_NAME)
SD_MOD_GUIDE_BASE := $(SD_PREFIX)-$(SD_MOD_GUIDE_NAME)-$(SD_LANG)
SD_MOD_GUIDE_SRC := $(SD_MOD_GUIDE_BASE).rst
SD_MOD_GUIDE_HTML := $(SD_MOD_GUIDE_BASE).html
SD_MOD_GUIDE_PDF := $(SD_MOD_GUIDE_BASE).pdf



SD_COUPLING_HOWTO_NAME := coupling-howto
SD_COUPLING_HOWTO_DIR := $(SD_COUPLING_HOWTO_NAME)
SD_COUPLING_HOWTO_BASE := $(SD_PREFIX)-$(SD_COUPLING_HOWTO_NAME)-$(SD_LANG)
SD_COUPLING_HOWTO_SRC := $(SD_COUPLING_HOWTO_BASE).rst
SD_COUPLING_HOWTO_HTML := $(SD_COUPLING_HOWTO_BASE).html
SD_COUPLING_HOWTO_PDF := $(SD_COUPLING_HOWTO_BASE).pdf



SD_DATAFLOW_HOWTO_NAME := dataflow-howto
SD_DATAFLOW_HOWTO_DIR := $(SD_DATAFLOW_HOWTO_NAME)
SD_DATAFLOW_HOWTO_BASE := $(SD_PREFIX)-$(SD_DATAFLOW_HOWTO_NAME)-$(SD_LANG)
SD_DATAFLOW_HOWTO_SRC := $(SD_DATAFLOW_HOWTO_BASE).rst
SD_DATAFLOW_HOWTO_HTML := $(SD_DATAFLOW_HOWTO_BASE).html
SD_DATAFLOW_HOWTO_PDF := $(SD_DATAFLOW_HOWTO_BASE).pdf



# Virtualisation section (Docker, Singularity, etc.)

# Generated from Dockerfile.template:
DOCKERFILE := Dockerfile

DOCKER_PREPARATION_SCRIPT := $(SIM_DIASCA_TOP)/conf/docker-images/prepare-docker-image.sh

DOCKER_IMAGE_BASE_NAME := sim-diasca

# Better than '.' (the current directory):
DOCKER_CONTEXT_NAME := docker-context

# Note that any registry specified must end with a slash:

# Possibly set from the user's environment: SIM_DIASCA_BASE_DOCKER_REGISTRY and
# SIM_DIASCA_ALT_DOCKER_REGISTRY.

# IMAGE_BASE_NAME to be set in each image-specific GNUmakefile.

IMAGE_SUFFIX = "$(DOCKER_IMAGE_BASE_NAME)/$(IMAGE_BASE_NAME)"


# SIM_DIASCA_{BASE,ALT}_DOCKER_REGISTRY possibly read from the environment,
# typically if using various internal registries:
#
IMAGE_BASE_TAGLESS = "$(SIM_DIASCA_BASE_DOCKER_REGISTRY)$(IMAGE_SUFFIX)"
IMAGE_ALT_TAGLESS = "$(SIM_DIASCA_ALT_DOCKER_REGISTRY)$(IMAGE_SUFFIX)"

IMAGE_BASE_REF_NAME = "$(IMAGE_BASE_TAGLESS):$(SIM_DIASCA_VERSION)"
IMAGE_BASE_LATEST_NAME = "$(IMAGE_BASE_TAGLESS):latest"

IMAGE_ALT_REF_NAME = "$(IMAGE_ALT_TAGLESS):$(SIM_DIASCA_VERSION)"
IMAGE_ALT_LATEST_NAME = "$(IMAGE_ALT_TAGLESS):latest"

# No tag to specify here:
IMAGE_BASE_PUSH_TARGET = "$(IMAGE_BASE_TAGLESS)"
IMAGE_ALT_PUSH_TARGET = "$(IMAGE_ALT_TAGLESS)"



# As we prefer having a Sim-Diasca specific message, denoting the use of its
# specific parse transform:
#
OVERRIDE_WOOPER_BUILD_RULES := true



# For later reuse in the next layer of the software stack:
SIM_DIASCA_PLT_FILE := $(SIM_DIASCA_TOP)/Sim-Diasca.plt


# This is the merged PLT of the level just below in the software stack.
# For Sim-Diasca, it is the PLT of 'Traces':
#
ifndef PREDECESSOR_PLT_FILE
	PREDECESSOR_PLT_FILE = $(TRACES_PLT_FILE)
endif


# We define the path to the Traces layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, dependencies will be searched
# (in that order, the one enforced in otp_utils) in a local _checkouts child
# directory (if checkout directories are used and if the top-level package of
# interest is the current one), otherwise in sibling directories (i.e. whose
# build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (ex: myriad) as opposed to
# project/repository names (ex: Ceylan-Myriad):
#
# First priority is an existing checkout directory:
ifneq ($(wildcard $(SIM_DIASCA_TOP)/_checkouts/traces/.),)
	TRACES_TOP = $(SIM_DIASCA_TOP)/_checkouts/traces
else
	# Otherwise a sibling one is searched for:
	ifneq ($(wildcard $(SIM_DIASCA_TOP)/../traces/.),)
		TRACES_TOP = $(SIM_DIASCA_TOP)/../traces
	else
		FORCE_EVAL := $(error Unable to find the build root of Ceylan-Traces)
	endif

endif

ifndef PREDECESSOR_LAYER_TOPS
	PREDECESSOR_LAYER_TOPS := $(TRACES_TOP)
endif
