# Prerequisite: MOCK_SIMULATORS_TOP must be set.

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

	$(error Invalid Mock-Simulators execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif



# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME = Mock-Simulators
endif


MOCK_SIMULATORS_VERSION := 1.0.1

# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION = $(MOCK_SIMULATORS_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME = mock-simulators
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP = $(MOCK_SIMULATORS_TOP)
endif


USE_PYTHON_BINDING = true

# Deactivated to avoid that the default build fails if Java is lacking:
#USE_JAVA_BINDING = true


MOCK_SIMULATORS_RELEASE_BASE := Mock-Simulators-$(PROJECT_VERSION)

MOCK_SIMULATORS_RELEASE_ARCHIVE_ZIP := $(MOCK_SIMULATORS_RELEASE_BASE).zip
MOCK_SIMULATORS_RELEASE_ARCHIVE_BZ2 := $(MOCK_SIMULATORS_RELEASE_BASE).tar.bz2


# Source section.
MOCK_SIMULATORS_SRC := $(MOCK_SIMULATORS_TOP)/ssi-test/src


# BEAM path section.
MOCK_SIMULATORS_BEAM := $(MOCK_SIMULATORS_TOP)/ssi-test/src

# Include path section.
MOCK_SIMULATORS_INC = -I$(MOCK_SIMULATORS_SRC)


INC += $(MOCK_SIMULATORS_INC)



ifndef VM_TEST_NAME
	VM_NAME := mock_simulators_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := mock_simulators_test
endif


BEAM_DIRS += $(MOCK_SIMULATORS_BEAM)


# For later reuse in the next layer of the software stack:
#MOCK_SIMULATORS_PLT_FILE := $(MOCK_SIMULATORS_TOP)/Mock-Simulators.plt

# This is the merged PLT of the level just below in the software stack.
# For all actual Mock Simulators, it is the PLT of Sim-Diasca:
#
ifndef PREDECESSOR_PLT_FILE
	PREDECESSOR_PLT_FILE = $(SIM_DIASCA_PLT_FILE)
endif


# We define the path to the Sim-Diasca layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, dependencies may be found
# either in a local _checkouts child directory (if checkout directories are used
# and if the top-level package of interest is the current one), or in sibling
# directories (i.e. whose build root is at the same level as this one).
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (ex: myriad) as opposed to
# project/repository names (ex: Ceylan-Myriad):
#
ifneq ($(wildcard $(MOCK_SIMULATORS_TOP)/../sim-diasca),)

	# By default, expect sibling directories:
	SIM_DIASCA_TOP = $(MOCK_SIMULATORS_TOP)/../sim-diasca

else

	# Otherwise assume a checkout one:
	ifneq ($(wildcard $(MOCK_SIMULATORS_TOP)/_checkouts/sim-diasca),)

		SIM_DIASCA_TOP = $(MOCK_SIMULATORS_TOP)/_checkouts/sim-diasca

   else

		FORCE_EVAL := $(error Unable to find the build root of Sim-Diasca)

	endif

endif
