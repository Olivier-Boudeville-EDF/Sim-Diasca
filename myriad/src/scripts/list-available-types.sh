#!/bin/sh

# Copyright (C) 2011-2023 Olivier Boudeville
#
# This file is part of the Ceylan-Myriad library.


usage="Usage: $(basename $0) [-h|--help] [ROOT_DIR]: lists all types (according to the Erlang type specifications found) defined from the ROOT_DIR directory (if specified), otherwise from the current directory."


target_dir="$(pwd)"

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"
	exit 0

fi


if [ -n "$1" ]; then

	target_dir="$1"
	shift

fi


if [ ! $# -eq 0 ]; then
	echo "  Error, up to one parameter allowed.
${usage}" 1>&2
	exit 10
fi


if [ ! -d "${target_dir}" ];  then
	echo "  Error, '${target_dir}' is not an existing directory.
${usage}" 1>&2
	exit 15
fi

echo "

  Searching for all Erlang types being defined from '${target_dir}':
"

cd "${target_dir}"

target_files=$(find . -name '*.hrl' -o -name '*.erl')

#echo "target_files = ${target_files}"

if [ -z "${target_files}" ]; then
	echo "No Erlang source file found."
	exit 0
fi


for f in ${target_files}; do

	#echo " + examining $f"

	# Complex: first removes the newlines (thus the file becomes a single line,
	# so that a type declaration cannot be broken into multiple parts anymore),
	# then print only the matching pattern, i.e. anything which start with
	# either '-type' or '-opaque' and end with the first dot found.
	# Finally, removes extra whitespaces.
	#
	# Removing newlines disallows the use of '^[:space]*' as header here:
	#
	# (there is at least one space before '-type', thus strings like
	# 'content-type' can be filtered out; if having '0..14.' we want to stop
	# after 14, not after the '0.', thus the 'OR ..' clause; same thing for
	# [warning(),...])
	#
	res=$(/bin/cat "$f" | tr '\n' ' ' | grep -o -E '[[:space:]]+-(type|opaque)([^.]|\.\.|\.\.\.)*\.' | sed -r 's|\s+| |g')

	if [ -n "${res}" ]; then

		echo "
   - specifications in $f:
${res}"

	fi

done
