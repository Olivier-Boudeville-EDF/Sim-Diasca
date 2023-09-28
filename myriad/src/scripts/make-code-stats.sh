#!/bin/sh

# Copyright (C) 2010-2023 Olivier Boudeville
#
# Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]

# This file is part of the Ceylan-Myriad library.


usage="Usage: $(basename $0) [-h|--help] [--lang LANGUAGE] [SOURCE_DIRECTORY]: evaluates various simple metrics of the code found from any specified root directory, otherwise from the current one.
Default language is 'Erlang'. Other supported languages are: 'Java'.
"

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "  ${usage}"
	exit

fi



lang="Erlang"

if [ "$1" = "--lang" ]; then

	lang="$2"
	shift
	shift

fi

case ${lang} in

	"Erlang")
		;;

	"Java")
		;;

	*)
		echo "  Error, unsupported language '${lang}'." 1>&2
		exit 10

esac

#echo "Selected language: '${lang}'."

bc="$(which bc 2>/dev/null)"

if [ ! -x "${bc}" ]; then

	echo "  Error, 'bc' command not found. Hint: on Arch Linux, use 'pacman -S bc' as root." 1>&2

	exit 5

fi

if [ -z "$1" ]; then

	source_dir="$(pwd)"

else

	source_dir="$1"
fi

root_dir="$(realpath ${source_dir})"

if [ -z "${root_dir}" ]; then

	echo "  Error, no root directory specified.
${usage}" 1>&2
	exit 10

fi


if [ ! -d "${root_dir}" ]; then

	echo "  Error, specified root directory (${root_dir}) does not exist.
${usage}" 1>&2
	exit 15

fi


if [ -n "$2" ]; then

	echo "  Error, an argument is lacking.
${usage}" 1>&2
	exit 20

fi


cd "${root_dir}"

# Sometimes in /bin, sometimes in /usr/bin, etc.:
find="$(which find 2>/dev/null)"
wc="$(which wc 2>/dev/null)"
expr="$(which expr 2>/dev/null)"

cat="/bin/cat"
grep="/bin/grep"


# We used to use -L to follow symlinks (not desirable here, not wanting to evade
# from the target source tree)
#
# Only regular files are selected, as includes in a tree may be symlinked in a
# top-level 'include' directory, and we do not want them to be counted more than
# once.


tmp_file=".tmp-code-stats.txt"

if [ -f "${tmp_file}" ]; then
	/bin/rm -f "${tmp_file}"
fi


if [ "${lang}" = "Erlang" ]; then


	# Rebar-related extra roots ('./_*', like _build, _checkouts, etc.) are
	# excluded.
	#
	hrl_files=$(${find} . \( -type f -o -path './_*' -prune \) -a -name '*.hrl' -print)
	erl_files=$(${find} . \( -type f -o -path './_*' -prune \) -a -name '*.erl' -print)

	#echo "hrl_files = ${hrl_files}"
	#echo "erl_files = ${erl_files}"

	hrl_count="$(echo ${hrl_files} | ${wc} -w)"
	erl_count="$(echo ${erl_files} | ${wc} -w)"

	target_files="${hrl_files} ${erl_files}"


	if [ "${target_files}" = " " ]; then

		echo "  Error, no Erlang source file found from '${root_dir}'." 1>&2
		exit 25

	fi

elif [ "${lang}" = "Java" ]; then

	java_files=$(${find} . \( -type f -o -path './_*' -prune \) -a -name '*.java' -print)

	#echo "java_files = ${java_files}"

	java_count="$(echo ${java_files} | ${wc} -w)"

	target_files="${java_files}"

	if [ "${target_files}" = " " ]; then

		echo "  Error, no Java source file found from '${root_dir}'." 1>&2
		exit 26

	fi

fi

#echo "target_files = ${target_files}"


# Very basic, yet simple and reliable:
for f in ${target_files}; do

	${cat} "$f" >> "${tmp_file}"

done


full_line_count=$(${cat} "${tmp_file}" | wc -l)

if [ $full_line_count -eq 0 ]; then

	echo "  Error, no ${lang} source code found from '${root_dir}'." 1>&2
	exit 35

fi


empty_line_count=$(${cat} "${tmp_file}" | ${grep} '^$' | ${wc} -l)
comment_line_count=$(${cat} "${tmp_file}" | ${grep} '^[[:space:]]*%' | ${wc} -l)


code_line_count=$(${expr} ${full_line_count} - ${empty_line_count} - ${comment_line_count})

empty_percentage=$(echo "scale=1; 100 * ${empty_line_count} / ${full_line_count}" | ${bc})
comment_percentage=$(echo "scale=1; 100 * ${comment_line_count} / ${full_line_count}" | ${bc})
code_percentage=$(echo "scale=1 ; 100 * ${code_line_count} / ${full_line_count}" | ${bc})

echo "In the ${lang} source code found from ${root_dir}, we have:"

if [ "${lang}" = "Erlang" ]; then

	echo "  + ${erl_count} source files (*.erl), ${hrl_count} header files (*.hrl)"

elif [ "${lang}" = "Java" ]; then

	echo "  + ${java_count} source files (*.java)"

fi

echo "  + a grand total of ${full_line_count} lines:"

echo "    - ${empty_line_count} of which (${empty_percentage}%) are blank lines"

if [ "${lang}" = "Erlang" ]; then

	echo "    - ${comment_line_count} of which (${comment_percentage}%) are comments"
	echo "    - ${code_line_count} of which (${code_percentage}%) are code"

fi

if [ -f "${tmp_file}" ]; then
	/bin/rm -f "${tmp_file}"
fi
