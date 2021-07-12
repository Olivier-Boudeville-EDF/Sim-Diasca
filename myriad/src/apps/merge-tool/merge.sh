#!/bin/sh

# Main launcher of the merge tool.

base_dir="$(pwd)"

merge_dir="$(dirname $0)"

#echo "base_dir = ${base_dir}, merge_dir = ${merge_dir}"

cd "${merge_dir}"

# Using --nn to spawn a non-named VM (i.e. a non-distributed node), to allow for
# multiple instances of this script/application to run concurrently.
#
# Any argument(s) specified to this script shall be interpreted as a plain,
# extra one:
#
make -s merge_utils.beam merge_exec NODE_NAMING="--nn" CMD_LINE_OPT="--base-dir ${base_dir} $*"
