#!/bin/sh

# Copyright (C) 2010-2021 Olivier Boudeville

# This file is part of the Ceylan-Myriad library.

# Note: now etop is deprecated in favor of observer.
# (not sure observer can be run in a terminal, in text mode)


usage="Usage: $(basename $0) [-node NODE_NAME] [-setcookie COOKIE]: shows on the console the activity of the Erlang processes on specified Erlang node (enter CTRL-C twice to exit).
  Example: etop.sh -node foobar@baz.org -setcookie 'my cookie'"

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "  ${usage}"
	exit

fi

if [ -z "$*" ]; then

	echo
	echo "(you can also specify a node and/or a cookie; use the --help option for more details)"
	echo

fi


# Ex for testing: erl -name foobar@baz.org

etop_base=$(which erl|sed 's|bin/erl$|lib/erlang/lib/observer-|1')

actual_etop_base=$(/bin/ls ${etop_base}* -d|tail -n 1)

#echo "actual_etop_base = $actual_etop_base"

etop="${actual_etop_base}/priv/bin/etop"

# The '-output graphical' option is not used anymore, as observer supersedes it:
${etop} -interval 1 -lines 20 $*
