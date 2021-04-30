#!/bin/sh

usage="  Usage: $(basename $0) [-h|--help] A_RECORD_NAME [A_DIR]: attempts to find the definition of the specified Erlang record from the specified directory (if any), otherwise from the current one."


if [ "$1" = "-h" ] || [ "$1" = "--help" ] || [ -z "$1" ]; then

	echo "  ${usage}"
	exit

fi

base_dir="$(pwd)"

record_name="$1"

if [ $# -eq 2 ]; then

	base_dir="$2"

	if [ ! -d "${base_dir}" ]; then

		echo "  Error, specified base search directory (${base_dir}) does not exist." 1>&2
		exit 10

	fi

	shift

fi

if [ ! $# -eq 1 ]; then

	echo "Error, invalid number of parameters.
${usage}" 1>&2
	exit 5

fi


echo "Looking for the definition of the '${record_name}' Erlang record from '${base_dir}':"
echo


# Finally better with no context (file will probably have to be opened anyway):
#context_opt="--after-context=15"
context_opt=""

# DUMMY to force the display of the corresponding file.
cd ${base_dir} && find . -name '*.?rl' -exec /bin/grep --after-context=50 --color ${context_opt} -e "[[:space:]]\?-record([[:space:]]\+${record_name}" DUMMY '{}' ';' 2>/dev/null
