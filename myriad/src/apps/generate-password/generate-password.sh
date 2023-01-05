#!/bin/sh

# Main launcher of the Ceylan-Myriad password generation tool (knowing that the
# escript-based version is a pain to debug in an escript form).

# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:

# To debug:
# Not relevant: make -s generate_password_exec CMD_LINE_OPT="-extra $*"
#make -s generate_password_exec CMD_LINE_OPT="$*"

#cd ${CEYLAN_MYRIAD} && make -s all 1>/dev/null && cd src/apps/generate-password && generate-password.escript $*

#cd ${CEYLAN_MYRIAD} && make -s all && cd src/apps/generate-password && generate-password.escript $*

# Now not triggering a prior recompilation check, expecting generally to be
# already built:
#
cd ${CEYLAN_MYRIAD} && cd src/apps/generate-password && generate-password.escript $*
