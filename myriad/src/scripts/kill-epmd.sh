#!/bin/sh

usage="Usage: $(basename $0) [-h|--help]: terminates, otherwise kills, all EPMD daemons found."

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit

fi


# 'epmd -kill' for the port(s) of interest could be another option, yet this
# script is mistly useful when EPMD wrongly believe that an Erlang node is still
# alive.


epmd_pids="$(ps -edf | grep 'epmd -daemon' | grep -v grep | awk '{print $2}')"


for target_pid in ${epmd_pids}; do

	kill ${target_pid}

done

# Hopefully enough:
sleep 1

epmd_pids="$(ps -edf | grep 'epmd -daemon' | grep -v grep | awk '{print $2}')"

if [ -z "${epmd_pids}" ]; then

	echo "No more EPMD process detected."

else

	echo "(EPMD processes ${epmd_pids} survived the TERM signal; killing them for good)"

fi

for target_pid in ${epmd_pids}; do

	kill -9 "${target_pid}" && echo "Killed!"

done
