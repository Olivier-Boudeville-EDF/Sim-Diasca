#!/bin/sh

tool="LogMX"

help_short_opt="-h"
help_long_opt="--help"

usage="Usage: $(basename $0) [${help_short_opt}|${help_long_opt}]: kills any interactive tool that was launched by any prior simulation."


if [ "$1" = "${help_short_opt}" ] || [ "$1" = "${help_long_opt}" ]; then

	echo "${usage}"

	exit 0

fi


if [ ! $# -eq 0 ]; then

	echo "  Error, extra argument specified.
${usage}" 1>&2

	exit 10

fi


kill_trace_tool_name="kill-trace-supervision-tool.sh"

kill_trace_tool="$(which ${kill_trace_tool_name} 2>/dev/null)"

if [ ! -x "${kill_trace_tool}" ]; then

	echo "  Error, the tool to kill any trace supervision instance ('${kill_trace_tool_name}') could not be found." 1>&2

	exit 55

fi


"${kill_trace_tool}"

killall geeqie
