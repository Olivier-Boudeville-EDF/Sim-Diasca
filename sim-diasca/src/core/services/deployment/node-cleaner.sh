#!/bin/sh

USAGE="
Usage: $(basename $0) [ NODE_NAME | [-h|--help] ]

This script will reliably kill, on the host on which it is run, any lingering Erlang node named NODE_NAME and owned by the current user, regardless of its cookie."

# Note: the previous versions of this script featured a --all option and
# cookie-based selection, but now we basically need only to remove any node with
# the target name.


# Uncomment to test whether this script is actually executed:
#echo "Executed!" > ~/node-cleaner-has-been-executed

#echo "Test for standard output writing"
#echo "Test for error output writing" 1>&2

# Test for error exit codes: exit 57

if [ -z "$1" ] ; then

	echo "Error, no argument specified. $USAGE" 1>&2
	exit 2

fi


nodename="$1"


if [ -z "$nodename" ] ; then

	echo "Error, node name not specified. $USAGE" 1>&2
	exit 10

fi


if [ ! $# -eq 1 ] ; then

	echo "Error, exactly one parameter (the node name) expected. $USAGE" 1>&2
	exit 15

fi

if [ $nodename = "-h" ] ||  [ $nodename = "--help" ] ; then

	echo "$USAGE"
	exit 0

fi


echo "Will kill all lingering Erlang nodes on "$(hostname)", if owned by $USER, and having a node name matching '$nodename'."


for p in $(/bin/ps -o pid,cmd -u $USER | grep "$nodename" | grep beam | grep -v grep | awk '{ print $1 }') ; do

	echo "  Killing process $p."
	kill $p

done


for p in $(/bin/ps -o pid,cmd -u $USER | grep "$nodename" | grep beam | grep -v grep | awk '{ print $1 }') ; do

	echo "  Force-killing process $p."
	kill -9 $p

done

found=1

for p in $(/bin/ps -o pid,cmd -u $USER| grep "$nodename" | grep beam | grep -v grep | awk '{ print $1 }') ; do

	echo "  Unable to kill process $p."
	found=0

done

if [ $found -eq 1 ] ; then

	echo "  No node '$nodename' exists anymore."

else

	echo "  Node '$nodename' could not be removed." 1>&2

fi


# Note: under certain circumstances, when an application crashes, the epmd
# daemon may still hold the node name, and thus will refuse that application to
# be launched again with the same name.


# To force the unregistering of a node name (outputs either NOEXIST or STOPPED):
# (post-R14A)
#
# Seems to work even if -relaxed_command_check was not specified at epmd
# creation (implicit automatic creation).
#
#
epmd -stop "$nodename"

res="$?"

echo "epmd stopped with return code $res."

# Killing epmd would solve the problem, but it would be a bad practise as any
# other Erlang application running on the same node would be affected.
# kill epmd

# However there might be cases (ex: relaunching a simulation after a crash)
# where the previous VM died (and thus does not exist as a UNIX process anymore)
# yet is still registered in epmd; ex:
#
# $ epmd -port 4506 -names
# epmd: up and running on port 4506 with data:
# name Data_Exchange_test-boudevil at port 50001

# Then a newly launched VM with the same name would crash (with
# {error_logger,[...]},"Protocol: ~tp: the name Data_Exchange_test-boudevil
# seems to be in use by another Erlang node",["inet_tcp"]}) while the name would
# still remain in epmd.

# By default 'epmd -port 4506 -stop Data_Exchange_test-boudevil' will actually
# *not* remove that name from epmd (tested with 'epmd -port 4506 -names'
# afterwards): as mentioned in http://erlang.org/doc/man/epmd.html the relaxed
# command check needs to be enabled for that.
#
# Rather than launching explicitly epmd (with the -relaxed_command_check
# option), we prefer to set the ERL_EPMD_RELAXED_COMMAND_CHECK environment
# variable beforehand (see launch-erl.sh)


# So that we can solely rely on the exit code:
exit 0
