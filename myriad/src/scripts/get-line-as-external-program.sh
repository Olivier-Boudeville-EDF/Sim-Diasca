#!/bin/sh

# Copyright (C) 2010-2021 Olivier Boudeville
#
# Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
#
# This file is part of the Ceylan-Myriad library.


# Typically used internally to Myriad, by system_utils:get_line/1.

# This (most simple) script allows to collect user inputs, as lines entered
# thanks to the keyboard (ex: for text_ui.erl) even if the Erlang VM is run with
# the -noinput setting, which apparently is necessary for other forms of user
# interactions such as /bin/dialog (see term_ui.erl).


# No prompt actually allowed, as no writing to the "standard" output can/should
# be done here (file descriptor 1, actually redirected to 4).

#prompt="$1"
#
#if [ -n "${prompt}" ]; then
#	echo "prompt: $1"
#fi

read res

echo "${res}"
