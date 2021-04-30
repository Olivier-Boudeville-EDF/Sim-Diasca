#!/bin/sh

usage="Usage: $(basename $0) [-h|--help] A_TYPE [A_DIR]: attempts to find the definition of the specified Erlang type from the specified directory (if any), otherwise from the current one."


if [ "$1" = "-h" ] || [ "$1" = "--help" ] || [ -z "$1" ]; then

	echo "  ${usage}"
	exit

fi

base_dir="$(pwd)"

type="$1"

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

base_dir="$(realpath ${base_dir})"

echo "Looking for the '${type}' Erlang type, from '${base_dir}':"
echo


# DUMMY to force the display of the corresponding file.
# '{type}', not '{type}()', so that partial type names can still be found.
#

cd ${base_dir} && find . -name '*.?rl' -exec /bin/grep --after-context=4 --color -e "[[:space:]]\?-\(type\|opaque\)[[:space:]]\+${type}" DUMMY '{}' ';' 2>/dev/null
