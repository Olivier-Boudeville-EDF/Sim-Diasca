#!/bin/sh


# Main launcher of the password generation tool (knowing that the escript-based
# version is a pain to debug in an escript form).

# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:

# To debug:
#make -s generate_password_exec CMD_LINE_OPT="-extra $*"

cd ${CEYLAN_MYRIAD} && make -s all 1>/dev/null && cd src/apps/generate-password && generate-password.escript $*
