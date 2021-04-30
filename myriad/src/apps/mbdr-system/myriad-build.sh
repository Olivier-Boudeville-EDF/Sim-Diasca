#!/bin/sh


# Main launcher of the Myriad build tool, part of the %BDR system.

# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:

# To debug:
#make -s myriad_build_exec CMD_LINE_OPT="-extra $*"

cd ${CEYLAN_MYRIAD} && make -s all 1>/dev/null && cd src/apps/mbdr-system && myriad-build.escript $*
