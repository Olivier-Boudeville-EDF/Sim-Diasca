#!/bin/sh

# Main launcher of the merge tool.

base_dir="$(pwd)"

merge_dir="$(dirname $0)"

#echo "base_dir = ${base_dir}, merge_dir = ${merge_dir}"

cd "${merge_dir}"

# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:
#
make -s merge_exec CMD_LINE_OPT="--base-dir ${base_dir} $*"
