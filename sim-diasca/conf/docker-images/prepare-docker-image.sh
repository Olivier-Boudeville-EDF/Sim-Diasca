#!/bin/sh

doc_opt="--for-doc"
dev_opt="--for-dev"

erl_version_opt="--erlang-version"

usage="Usage: $(basename $0) [${doc_opt}|${dev_opt}] [${erl_version_opt} ERL_VERSION]: prepares all elements needed in order to generate a suitable Sim-Diasca Docker image, the base runtime image relying on the specified Erlang version, the others inheriting from it.

 By default, a base, freely-redistributable runtime image will be prepared.

 Other images shall not be shared externally:
   - if the ${doc_opt} option is specified, an image for the generation of
	 documentation will be prepared instead (only of use for internal GitLab)
   - if the ${dev_opt} option is specified, a richer image for development will
	 be prepared instead, comprising by default a LogMX licence file
"

# Typically to be executed from an image-specific subdirectory of
# sim-diasca/conf/docker-images (through a GNUmakefile).


# Stop on error:
set -e

# Stop on unitialized variables (not used as $1 may not be set):
#set -u

# Stop on failed pipes (not always supported apparently):
#set -o pipefail


# General settings

# Debian 10 currently:
os_base_image="debian:buster"

# If using a public repository:
#os_image="${os_base_image}"


# If using an internal registry (possibly not defined; possibly including a port
# number, like in 'nexus.foobar.org:5012'):
#
internal_repository="${SIM_DIASCA_BASE_DOCKER_REGISTRY}"

os_image="${internal_repository}${os_base_image}"


# The directory-like structure in which all Sim-Diasca are declared:
sd_group="sim-diasca"


# The version tag to request when the current image is to derive from another
# Sim-Diasca image:
#
sd_tag="latest"


# Only prepared for the base runtime image; inherited by the other images:
prepare_erlang=1


# Currently always enabled, at least so that the README of each image records
# the right version of Sim-Diasca:
#
prepare_sd=0


# Only prepared for the development image:
#
# (warning: if selecting a professional LogMX version, will include a non-public
# licence file; otherwise no point in including LogMX at all)
#
prepare_logmx=1


# If wanting extra user facilities (ex: packages, shell configuration):
prepare_extra_env=1


if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then

	echo ${usage}

	exit

fi


# Types of images:
#   1: base runtime
#   2: for doc generation (deriving from 1)
#   3: for developers (deriving from 2)

# Defaults:
image_type=1
image_description="base runtime"
image_name="base-runtime"

if [ "$1" = "${doc_opt}" ]; then

	if [ -z "${SIM_DIASCA_INTERNAL_DOC_CLONE_ROOT}" ]; then

		echo "  Error, no SIM_DIASCA_INTERNAL_DOC_CLONE_ROOT environment variable defined." 1>&2

		exit 25

	fi


	if [ ! -d "${SIM_DIASCA_INTERNAL_DOC_CLONE_ROOT}" ]; then

		echo "  Error, Sim-Diasca internal clone root ('${SIM_DIASCA_INTERNAL_DOC_CLONE_ROOT}', designated by the SIM_DIASCA_INTERNAL_DOC_CLONE_ROOT environment variable, does not exist." 1>&2

		exit 30

	fi

	image_type=2
	image_description="documentation generation"
	image_name="documentation-generation"

	shift

fi


if [ "$1" = "${dev_opt}" ]; then

	image_type=3
	image_description="development"
	image_name="development"

	shift

fi


if [ "$1" = "${erl_version_opt}" ]; then
	shift
	erl_version="$1"
	#echo "(setting Erlang version to ${erl_version})"
	shift
fi

if [ ! $# -eq 0 ]; then

	echo "  Error, invalid parameter(s) specified.
${usage}" 1>&2

	exit 5

fi



context_dir="docker-context"

# Expected to already exist, as containing static files of interest:
if [ ! -d "${context_dir}" ]; then

	echo "  Error, context directory '${context_dir}' not found.
${usage}" 1>&2

	exit 6

fi


cd "${context_dir}"

dockerfile="Dockerfile"
dockerfile_template="${dockerfile}.template"

if [ ! -f "${dockerfile_template}" ]; then

	echo "  Error, Dockerfile template ('${dockerfile_template}') not found from context in $(pwd)." 1>&2

	exit 10

fi


image_readme="README-About-This-Docker-Image.txt"
image_readme_template="${image_readme}.template"

if [ ! -f "${image_readme_template}" ]; then

	echo "  Error, image README template ('${image_readme_template}') not found from context in $(pwd)." 1>&2

	exit 11

fi


echo

case $image_type in

	1)
		parent_image="${os_image}"
		prepare_erlang=0
		# Implied: prepare_sd=0
		echo "Preparing a base runtime Sim-Diasca Docker image, deriving from image '${parent_image}'..."
		;;

	2)
		parent_image="${internal_repository}${sd_group}/base-runtime:${sd_tag}"
		echo "Preparing a Sim-Diasca Docker image for documentation generation, deriving from image '${parent_image}'...."
		;;

	3)
		parent_image="${internal_repository}${sd_group}/documentation-generation:${sd_tag}"
		prepare_logmx=0
		prepare_extra_env=0
		echo "Preparing a Sim-Diasca Docker image for development, deriving from image '${parent_image}'...."
	   ;;

	*)
		echo "Unexpected image type."
		exit 25
		;;

esac



if [ -f "${dockerfile}" ]; then

	echo " - removing older '${dockerfile}'"
	/bin/rm -f "${dockerfile}"

fi


if [ $prepare_erlang -eq 0 ]; then

	if [ -z "${erl_version}" ]; then

		echo "  Error, no Erlang version defined (see the ${erl_version_opt} command-line option).
${usage}" 1>&2

		exit 45

	fi

	echo " - securing sources of Erlang ${erl_version}"

	# Nothing done if already available, otherwise copied from local reference:
	erl_src_archive_name="otp_src_${erl_version}.tar.gz"

	erl_src_archive_path="${HOME}/Software/Erlang/${erl_src_archive_name}"

	# We are still in docker-context:
	if [ ! -f "${erl_src_archive_name}" ]; then

		if [ ! -f "${erl_src_archive_path}" ]; then

			echo "  Error, no reference Erlang source archive found (searched as '${erl_src_archive_path}')." 1>&2

			exit 75

		fi

		# Docker does not support symlinks on purpose (not repeatable), so:
		/bin/cp -f "${erl_src_archive_path}" .


		# Removing any older archive(s):
		for f in $(/bin/ls -1 otp_src_*.tar.gz 2>/dev/null); do

			if [ "$f" != "${erl_src_archive_name}" ]; then

				if [ ! -e "$f" ]; then
					echo "  (removing archive '$f' of unmatching version)"
					/bin/rm -f "$f"

				fi

			fi

		done

	fi

fi


# Needed in all cases:



if [ $prepare_sd -eq 0 ]; then

	# The sources of Sim-Diasca will be obtained by the image thanks to a Docker
	# context, rather than directly by cloning (more flexible in terms of Git
	# origin/version/branch):

	sd_clone_root="Sim-Diasca"


	if [ ! -d "${sd_clone_root}" ]; then

		# The default Git is the public one:
		SIM_DIASCA_GIT="https://github.com/Olivier-Boudeville-EDF/Sim-Diasca.git"

		if [ -n "${SIM_DIASCA_INTERNAL_GIT}" ]; then
			echo " - cloning Sim-Diasca internal repository"
			SIM_DIASCA_GIT="${SIM_DIASCA_INTERNAL_GIT}"
		else
			echo " - cloning Sim-Diasca public repository"
		fi

		git clone "${SIM_DIASCA_GIT}" "${sd_clone_root}"

	else

		echo " - updating prior Sim-Diasca repository"
		cd "${sd_clone_root}" && git pull && cd ..

	fi

	# Used in multiple places:
	sd_version=$(/bin/cat "${sd_clone_root}/sim-diasca/GNUmakevars.inc" | grep 'SIM_DIASCA_BASE_VERSION :=' | sed 's|^SIM_DIASCA_BASE_VERSION := ||1')

fi


# Note that 'sd_version' is set iff prepare_sd is requested (set to 0):
if [ -z "${sd_version}" ]; then
	sd_version="(none)"
	echo "Warning: no Sim-Diasca version set, default to '${sd_version}'." 1>&2
fi

full_image_name="${image_name}:${sd_version}"

# Tag substitution matters, as some tag names are subsets of others.
# OPERATION_SYSTEM_TAG replaced directly from the image:
cat "${dockerfile_template}" | sed "s|FULL_IMAGE_NAME_TAG|${full_image_name}|g" | sed "s|IMAGE_NAME_TAG|${image_name}|g" | sed "s|IMAGE_DESCRIPTION_TAG|${image_description}|g" | sed "s|PARENT_IMAGE_TAG|${parent_image}|g" | sed "s|ERLANG_VERSION_TAG|${erl_version}|g" | sed "s|SIM_DIASCA_VERSION_TAG|${sd_version}|g" | sed "s|GENERATION_TIMESTAMP_TAG|$(LC_ALL= date '+%A, %B %-e, %Y, at %H:%M:%S')|g" > "${dockerfile}"


if [ $prepare_logmx -eq 0 ]; then

	# The expected general form is '~/Software/LogMX/LogMX_v?.?.?[_pro].zip',
	# depending on version number and whether using the 'pro' version
	# (recommended) or the 'free' one.

	logmx_base="${HOME}/Software/LogMX"

	# Selects the latest available version, preferably in its pro version:
	logmx_archive="$(/bin/ls -1 ${logmx_base}/LogMX_v?.?.*zip 2>/dev/null | tail -n 1)"

	if [ ! -e "${logmx_archive}" ]; then

		echo "  Error, unable to find a LogMX archive (no matching LogMX zip archive found in '${logmx_base}')." 1>&2

		exit 60

	fi

	/bin/cp -f "${logmx_archive}" .

	logmx_install="${logmx_base}/LogMX-current-install"

	logmx_licence_file="${logmx_install}/config/license.properties"

	if [ -f "${logmx_licence_file}" ]; then

		/bin/cp -f "${logmx_licence_file}" .

	else

		echo "  Warning: unable to find a LogMX licence file (no '${logmx_licence_file}')." 1>&2

		# Hopefully using the evaluation version then.

		#exit 65

	fi

	for f in logging.properties logmx.properties managers.properties parsers.properties; do
		/bin/cp -f "${logmx_install}/config/$f" .
	done

	/bin/cp -f "${logmx_install}/parsers/classes/ceylan/parser/CeylanTraceParser.class" .

fi


if [ $prepare_extra_env -eq 0 ]; then

	# VM_INTERNAL_EXTRA_PACKAGES possibly defined in environment:
	sed -i "s|EXTRA_PACKAGES_TAG|${VM_INTERNAL_EXTRA_PACKAGES}|g" "${dockerfile}"

	# Convenient as well:
	/bin/cp -f ~/.bashrc ~/.bashrc.basics ~/.bashrc.Linux ~/.bashrc.final . 2>/dev/null || true

fi


cat "${image_readme_template}" | sed "s|FULL_IMAGE_NAME_TAG|${full_image_name}|g" | sed "s|IMAGE_NAME_TAG|${image_name}|g" | sed "s|IMAGE_DESCRIPTION_TAG|${image_description}|g" | sed "s|PARENT_IMAGE_TAG|${parent_image}|g" | sed "s|ERLANG_VERSION_TAG|${erl_version}|g" | sed "s|SIM_DIASCA_VERSION_TAG|${sd_version}|g" | sed "s|GENERATION_TIMESTAMP_TAG|$(LC_ALL= date '+%A, %B %-e, %Y, at %H:%M:%S')|g" > "${image_readme}"


echo
echo "The Dockerfile '${context_dir}/${dockerfile}' and its context are ready for Sim-Diasca version ${sd_version}, and a corresponding (anonymous, untagged) image may be created from it thanks to: 'sudo docker build docker-context'."
