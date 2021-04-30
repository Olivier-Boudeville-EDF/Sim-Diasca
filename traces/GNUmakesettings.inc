# This is the single file that all Traces GNUmakefiles, and only them, should
# include.

# Prerequisite: TRACES_TOP must be set.


# Each project should define its own GNUmakesettings.inc, as explained in
# myriad/GNUmakesettings.inc.


# Nothing simpler can be used:


# First the make variables:

include $(TRACES_TOP)/GNUmakevars.inc
include $(WOOPER_TOP)/GNUmakevars.inc
include $(MYRIAD_TOP)/GNUmakevars.inc


# Then rules are split between automatic (generic, pattern-based) ones and
# explicit (immediate, static) ones, so that the latter can be placed last
# (otherwise they would shadow any default target, such as 'all', defined by any
# lower layer, for instance when overriding base cleaning with their own
# 'clean-local' target):

include $(TRACES_TOP)/GNUmakerules-automatic.inc
include $(WOOPER_TOP)/GNUmakerules-automatic.inc
include $(MYRIAD_TOP)/GNUmakerules-automatic.inc

include $(TRACES_TOP)/GNUmakerules-explicit.inc
include $(WOOPER_TOP)/GNUmakerules-explicit.inc
include $(MYRIAD_TOP)/GNUmakerules-explicit.inc
