#!/bin/sh

tool="LogMX"

help_short_opt="-h"
help_long_opt="--help"

spare_graph_short_opt="-sg"
spare_graph_long_opt="--spare-graph-streaming-tool"

usage="Usage: $(basename $0) [${help_short_opt}|${help_long_opt}] [${spare_graph_short_opt}|${spare_graph_long_opt}]: kills any interactive tool that was launched by any prior simulation.
Options:
  ${spare_graph_short_opt} or ${spare_graph_long_opt}: do not kill the tool for graph streaming
"


spare_graph=1


if [ "$1" = "${help_short_opt}" ] || [ "$1" = "${help_long_opt}" ]; then

	echo "${usage}"

	exit 0

fi


if [ "$1" = "${spare_graph_short_opt}" ] || [ "$1" = "${spare_graph_long_opt}" ]; then

	echo "(any graph stream tool will be kept)"
	spare_graph=0
	shift
	
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


if [ $spare_graph -eq 1 ]; then

	kill_graph_streaming_tool_name="kill-graph-stream-tool.sh"

	kill_graph_streaming_tool="$(which ${kill_graph_streaming_tool_name} 2>/dev/null)"

	if [ ! -x "${kill_graph_streaming_tool}" ]; then

		echo "  Error, the tool to kill any graph streaming instance ('${kill_graph_streaming_tool_name}') could not be found." 1>&2

		exit 60

	fi

	"${kill_graph_streaming_tool}"

fi

killall geeqie
