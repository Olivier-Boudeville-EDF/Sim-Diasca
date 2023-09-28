#!/bin/sh

usage="Usage: $(basename $0) [-h|--help]: interactively terminates otherwise kills the user-selected Erlang (BEAM) virtual machines that were launched thanks to '-eval'."


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit

fi


if [ ! $# -eq 0 ]; then

	echo "  Error, no argument expected.
${usage}" 1>&2

	exit 5

fi


for target_pid in $(ps -edf | grep beam.smp | grep ' -eval' | while read ps_line; do

	pid="$(echo "${ps_line}" | awk '{print $2}')"
	echo "${pid}"

		   done ); do

	mfa="$(ps -p "${target_pid}" -o cmd= | sed 's|^.*eval ||1' | sed 's|\(\) .*$||1')"

	# Send the TERM signal:
	echo " - terminate the BEAM VM that has been launched with '${mfa}'? (PID: ${target_pid}) [y/N]"
	read answer

	if [ "${answer}" = "y" ]; then

		kill "${target_pid}"

		# Hopefully enough:
		sleep 1

		if ps "${target_pid}" 1>/dev/null 2>&1; then

			echo "Process survived TERM signal! Shall we kill it for good? [y/N]"
			read answer

			if [ "${answer}" = "y" ]; then

				kill -9 "${target_pid}" && echo "Killed!"

			else

				echo "(killed cancelled)"

			fi


		else

			echo "Terminated!"

		fi

	else

		echo "(termination cancelled)"

	fi

done
