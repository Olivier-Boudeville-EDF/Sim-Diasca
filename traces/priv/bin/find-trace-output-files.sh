#!/bin/sh

ext=".traces"

help_short_opt="-h"
help_long_opt="--help"

usage="Usage: $(basename $0) [${help_short_opt}|${help_long_opt}] [ROOT_DIR]: finds all trace output files (extension: '${ext}') from the specified root directory, otherwise from current one."


if [ "$1" = "${help_short_opt}" ] || [ "$1" = "${help_long_opt}" ]; then

	echo "${usage}"

	exit 0

fi


if [ -n "$1" ]; then

	root_dir="$(realpath $1)"

else

	root_dir="$(pwd)"

fi



#order_by="unsorted"
order_by="time"


if [ $order_by = "unsorted" ]; then

	echo "  Finding all '*${ext}' files from '${root_dir}':"

	find "${root_dir}" -name '*.traces'

elif [ $order_by = "time" ]; then

	echo "  Finding all '*${ext}' files from '${root_dir}', most recent first:"

	#find . -name '*.traces' -printf "%TY-%Tm-%Td %TH:%TM:%TS %p\n" | sort -n
	#find . -name '*.traces' -printf "%TY-%Tm-%Td %Tr %p\n" | sort -n
	find . -name '*.traces' -printf "%TY-%Tm-%Td %TT %p\n" | sort -rn

fi
