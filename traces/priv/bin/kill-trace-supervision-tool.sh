#!/bin/sh

tool="LogMX"

help_short_opt="-h"
help_long_opt="--help"

usage="Usage: $(basename $0) [${help_short_opt}|${help_long_opt}]: kills all instances, run by the current user on the local host, of the tool that is used to perform trace (log) supervision (namely ${tool})."


if [ "$1" = "${help_short_opt}" ] || [ "$1" = "${help_long_opt}" ]; then

	echo "${usage}"

	exit 0

fi


if [ ! $# -eq 0 ]; then

	echo "  Error, extra argument specified.
${usage}" 1>&2

	exit 10

fi


pids=$(ps -u ${USER} -eo pid,cmd | grep logmx.jar | grep -v grep | awk '{print $1}')

#echo "LogMX PIDs: ${pids}"

if [ -z "${pids}" ]; then

	echo " (no instance of ${tool} was found running for ${USER}@$(hostname))"

	exit 0

fi


for pid in ${pids}; do

	#echo "  Killing ${tool} instance of PID ${pid}"
	kill ${pid}

done

echo " ${tool} not expected to run anymore for ${USER}@$(hostname)."
