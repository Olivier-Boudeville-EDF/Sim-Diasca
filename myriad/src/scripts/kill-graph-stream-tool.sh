#!/bin/sh

tool="Gephi"

help_short_opt="-h"
help_long_opt="--help"

usage="Usage: $(basename $0) [${help_short_opt}|${help_long_opt}]: kills all instances, run by the current user on the local host, of the tool that is used to perform graph streaming (namely ${tool})."


if [ "$1" = "${help_short_opt}" ] || [ "$1" = "${help_long_opt}" ]; then

	echo "${usage}"

	exit 0

fi


if [ ! $# -eq 0 ]; then

	echo "  Error, extra argument specified.
${usage}" 1>&2

	exit 10

fi


# Typically to locate platform/lib/nbexec and bin/java -Djdk.home=...:
pids=$(ps -u ${USER} -eo pid,cmd | grep gephi | grep java | grep -v grep | awk '{print $1}')

#echo "Gephi PIDs: ${pids}"

if [ -z "${pids}" ]; then

	echo " (no instance of ${tool} was found running for ${USER}@$(hostname))"

	exit 0

fi

for pid in ${pids}; do

	#echo "  Killing ${tool} instance of PID ${pid}"
	kill ${pid}

done

echo " ${tool} not expected to run anymore for ${USER}@$(hostname)."

# Not necessary in our tests (regarding a risk of connection failure to a Gephi
# instance launched just afterwards, which was experienced with 'killall java'):
#
#sleep 1
