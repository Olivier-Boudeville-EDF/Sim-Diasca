# This is the single file that all Myriad GNUmakefiles, and only them, should
# include.

# Prerequisite: MYRIAD_TOP must be set.


# Each project should define its own GNUmakesettings.inc, as explained below,
# and should not reuse any other GNUmakesettings.inc coming from prerequisites.


# Note regarding dependencies and settings:

# let 'c' be 'myriad', 'b' be a library using 'myriad' (ex: 'wooper') and 'a' an
# application using the 'b' library: 'a' depends on 'b' that depends on 'c'.
#
# For any makefile of 'a', the correct include order would be:
#
#  - all GNUmakevars before all GNUmakerules
#
#  - among GNUmakevars, first 'a', then 'b', then 'c', as 'b' will define
# variables if 'a' did not define them, and so on
#
# - among GNUmakerules, first 'a', then 'b', then 'c', for the same reason:
# rules will be matched according to their declaration order (should all target
# stems have the same length)

#
# Therefore we need to have:
# a.GNUmakevars
# b.GNUmakevars
# c.GNUmakevars
# a.GNUmakerules
# b.GNUmakerules
# c.GNUmakerules

# Hence, for no project, the GNUmakevars and GNUmakerules should be aggregated.

# As it is a lot more convenient that all GNUmakefiles include only one global
# GNUmake*inc file, each project should define an appropriate one, which ought
# to be named 'GNUmakesettings.inc' (this file).

# Finally, GNUmakerules files have further been split into
# GNUmakerules-automatic.inc and GNUmakerules-explicit.inc, to let an upper
# layer define explicit rules (ex: when overriding base cleaning with a
# 'clean-local' target) without having them shadow any already-defined (in a
# lower layer) default target (such as 'all').

include $(MYRIAD_TOP)/GNUmakevars.inc


include $(MYRIAD_TOP)/GNUmakerules-automatic.inc

# For this single, base layer, nothing to insert here.

include $(MYRIAD_TOP)/GNUmakerules-explicit.inc
