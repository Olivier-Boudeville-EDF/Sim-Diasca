#!/bin/sh

# Now executing programs through -run rather than -eval:
#target_mode='eval'
target_mode='run'

usage="Usage: $(basename $0) [-h|--help] [-n|--non-interactive] [-epmd] [PROG_NAME]: interactively terminates otherwise kills the user-selected Erlang (BEAM) virtual machines that were launched thanks to '-${target_mode}'.

  Options (in that order):
	-n or --non-interactive: no user acknowledgement will be requested before killing (use with care!)
	-epmd: kills also any EPMD daemon found
	PROG_NAME: only the processes started as '-${target_mode} PROG_NAME' will be selected for killing

For example: $(basename $0) -n foobar_app
"

is_interactive=0
kill_epmds=1


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit

fi


if [ "$1" = "-n" ] || [ "$1" = "--non-interactive" ]; then

	#echo "(switching to non-interactive mode)"
	is_interactive=1
	shift

fi


if [ "$1" = "-epmd" ]; then

	#echo "(EPMD daemons will be killed as well)"
	kill_epmds=0
	shift

fi

prog_name="$1"

if [ $# -gt 1 ]; then

	echo "  Error, extra argument specified.
${usage}" 1>&2

	exit 5

fi


for target_pid in $(ps -edf | grep beam.smp | grep " -${target_mode} ${prog_name}" | while read ps_line; do

	pid="$(echo "${ps_line}" | awk '{print $2}')"
	echo "${pid}"

		   done ); do

	if [ $is_interactive eq 0 ]; then

		mfa="$(ps -p "${target_pid}" -o cmd= | sed "s|^.*${target_mode} ||1" | sed 's|\(\) .*$||1')"

		# Send the TERM signal:
		echo " - terminate the BEAM VM that has been launched as '${mfa}'? (PID: ${target_pid}) [y/N]"
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

	else

		kill "${target_pid}"

		# Hopefully enough:
		sleep 1

		if ps "${target_pid}" 1>/dev/null 2>&1; then

			echo "(process ${target_pid} survived TERM signal; killing it for good)"

			kill -9 "${target_pid}" && echo "Killed!"

		fi

	fi

done



if [ $kill_epmds -eq 0 ]; then

	epmd_pids="$(ps -edf | grep 'epmd -daemon' | grep -v grep | awk '{print $2}')"

	if [ -n "${epmd_pids}" ]; then

		echo "EPMD daemon(s) found; kill as well? [y/N]"
		read answer

		if [ "${answer}" = "y" ]; then

			for target_pid in ${epmd_pids}; do

				kill ${target_pid}

			done

			# Hopefully enough:
			sleep 1

			epmd_pids="$(ps -edf | grep 'epmd -daemon' | grep -v grep | awk '{print $2}')"

			if [ -n "${epmd_pids}" ]; then

				echo "(EPMD processes ${epmd_pids} survived TERM signal; killing them for good)"

			fi

			for target_pid in ${epmd_pids}; do

				kill -9 "${target_pid}" && echo "Killed!"

			done

		else

			echo "(termination cancelled)"

		fi

	else

		for target_pid in ${epmd_pids}; do

			kill ${target_pid}

		done

		# Hopefully enough:
		sleep 1

		epmd_pids="$(ps -edf | grep 'epmd -daemon' | grep -v grep | awk '{print $2}')"

		for target_pid in ${epmd_pids}; do

			kill -9 "${target_pid}" && echo "Killed!"

		done

	fi

fi
