#!/bin/sh

usage="Usage: $(basename $0) PROJECT_NAME [VERBOSE_MODE:0|1]"

project_name="$1"

# Not verbose by default (1):
verbose=1
#verbose=0

if [ -n "$2" ]; then
	verbose="$2"
fi


if [ -z "${project_name}" ]; then

	echo "  Error, project name not set." 1>&2

	exit 5

fi

# Removes the side effects in source tree of conf/fix-rebar-compile-pre-hook.sh.

echo "Fixing rebar post-build for ${project_name}, from $(pwd)."

to_rename=$(find src test -name '*.erl-hidden')

# Applies only if needed:

[ $verbose -eq 1 ] || echo "Renaming back ${to_rename}"

for f in ${to_rename}; do

	corrected_f=$(echo $f|sed 's|\.erl-hidden$|.erl|1')
	/bin/mv -f $f ${corrected_f}

done

echo "Rebar post-build fixed for ${project_name}."
