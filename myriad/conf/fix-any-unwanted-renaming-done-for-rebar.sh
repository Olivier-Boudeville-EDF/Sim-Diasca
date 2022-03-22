#!/bin/sh

suffix="-hidden-for-rebar3"

usage="Usage: $(basename $0) [TARGET_DIR]: fixes any lingering renaming (with the '${suffix}') of files done for rebar3, which was typically done by fix-rebar-compile-pre-hook.sh and was expected to be undone by fix-rebar-compile-post-hook.sh"

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit 0

fi

target_dir="$1"

if [ -z "${target_dir}" ]; then

	target_dir="$(pwd)"

fi

if [ ! -d "${target_dir}" ]; then

	echo "  Error, '${target_dir}' is not an existing directory." 1>&2

	exit 5

fi

echo "Removing all renamings done for rebar3 from '${target_dir}'..."

for f in $(find "${target_dir}" -name '*-hidden-for-rebar3'); do

	echo " - fixing $f"

	# Not -f to avoid any accidental overwriting:
	/bin/mv -f "$f" "$(echo $f | sed "s|${suffix}$||1")"

done
