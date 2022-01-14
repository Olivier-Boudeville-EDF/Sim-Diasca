#!/bin/sh

tool="LogMX"

usage="Usage: $(basename $0): kills all instances, run by the current user on the local host, of the tool that is used to perform trace (log) supervision (namely ${tool})."


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
