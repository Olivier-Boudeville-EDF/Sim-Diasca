# This is the single file that all GNUmakefiles of the full distribution - and
# only them - should include.

# Prerequisite: ROOT_TOP must be set.


# Each project should define its own GNUmakesettings.inc, as explained in
# myriad/GNUmakesettings.inc.


# Nothing simpler can be used:


# Starting with the make variables:

ifeq ($(HAS_SUSTAINABLE_CITIES),true)
	include $(SUSTAINABLE_CITIES_TOP)/GNUmakevars.inc
endif

ifeq ($(HAS_BLOCKCHAIN_SIM),true)
	include $(BLOCKCHAIN_SIM_TOP)/GNUmakevars.inc
endif

ifeq ($(HAS_PLANNING_SIM),true)
	include $(PLANNING_SIM_TOP)/GNUmakevars.inc
endif


include $(MOCK_SIMULATORS_TOP)/GNUmakevars.inc
include $(SIM_DIASCA_TOP)/GNUmakevars.inc
include $(TRACES_TOP)/GNUmakevars.inc
include $(WOOPER_TOP)/GNUmakevars.inc
include $(MYRIAD_TOP)/GNUmakevars.inc

include $(ROOT_TOP)/GNUmakevars.inc


# Then rules are split between automatic (generic, pattern-based) ones and
# explicit (immediate, static) ones, so that the latter can be placed last
# (otherwise they would shadow any default target, such as 'all', defined by any
# lower layer, for instance when overriding base cleaning with their own
# 'clean-local' target):


ifeq ($(HAS_SUSTAINABLE_CITIES),true)
	include $(SUSTAINABLE_CITIES_TOP)/GNUmakerules-automatic.inc
endif

ifeq ($(HAS_BLOCKCHAIN_SIM),true)
	include $(BLOCKCHAIN_SIM_TOP)/GNUmakerules-automatic.inc
endif

ifeq ($(HAS_PLANNING_SIM),true)
	include $(PLANNING_SIM_TOP)/GNUmakerules-automatic.inc
endif

include $(MOCK_SIMULATORS_TOP)/GNUmakerules-automatic.inc
include $(SIM_DIASCA_TOP)/GNUmakerules-automatic.inc
include $(TRACES_TOP)/GNUmakerules-automatic.inc
include $(WOOPER_TOP)/GNUmakerules-automatic.inc
include $(MYRIAD_TOP)/GNUmakerules-automatic.inc


ifeq ($(HAS_SUSTAINABLE_CITIES),true)
	include $(SUSTAINABLE_CITIES_TOP)/GNUmakerules-explicit.inc
endif

ifeq ($(HAS_BLOCKCHAIN_SIM),true)
	include $(BLOCKCHAIN_SIM_TOP)/GNUmakerules-explicit.inc
endif

ifeq ($(HAS_PLANNING_SIM),true)
	include $(PLANNING_SIM_TOP)/GNUmakerules-explicit.inc
endif

include $(MOCK_SIMULATORS_TOP)/GNUmakerules-explicit.inc
include $(SIM_DIASCA_TOP)/GNUmakerules-explicit.inc
include $(TRACES_TOP)/GNUmakerules-explicit.inc
include $(WOOPER_TOP)/GNUmakerules-explicit.inc
include $(MYRIAD_TOP)/GNUmakerules-explicit.inc
