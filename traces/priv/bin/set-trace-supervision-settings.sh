#!/bin/sh

usage="Usage: $(basename $0) [-h|--help] SETTING_CONTEXT: applies trace supervision settings that correspond to the specified context, where SETTING_CONTEXT is one of:
 - demo (thus with larger characters)
 - large (large screen, high resolution, so large characters are needed)
 - laptop (normal characters)
"

if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo "${usage}"

	exit

fi

if [ ! $# -eq 1 ]; then

	echo "  Error, exactly one parameter expected.
${usage}" 1>&2
	exit 20

fi

context="$1"

logmx_cfg_dir="${HOME}/Software/LogMX/LogMX-current-install/config"

if [ ! -d "${logmx_cfg_dir}" ]; then

	echo "  Error, the expected LogMX configuration directory, '${logmx_cfg_dir}', does not exist." 1>&2

	exit 25

fi


target_conf_file="logmx.properties"

case "${context}" in

	"demo")
		src_conf_file="${target_conf_file}-for-demo-larger-characters"
		;;

	"large")
		src_conf_file="${target_conf_file}-for-large-screen"
		;;

	"laptop")
		src_conf_file="${target_conf_file}-for-laptop-screen"
		;;

	*)
		echo "  Error, unknown settings context '${context}'.
${usage}" 1>&2
		exit 30

esac


cd "${logmx_cfg_dir}" || exit 31


if [ ! -f "${src_conf_file}" ]; then

	echo "  Error, content configuration file not found ('${src_conf_file}')." 1>&2

	exit 35

fi


if [ -e "${target_conf_file}" ]; then

	# Overwrites any prior backup:
	/bin/mv -f "${target_conf_file}" "${target_conf_file}.bak"

fi

ln -s "${src_conf_file}" "${target_conf_file}"

echo "Trace supervision settings successfully updated for the ${context} context."
