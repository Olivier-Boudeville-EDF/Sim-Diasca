#!/bin/sh

usage="Usage: $(basename $0) PROJECT_NAME [VERBOSE_MODE:0|1]"

# Script defined for convenience and reliability.

# Recreates a proper rebar3 landscape, based on our build, so that rebar3 will
# not attempt (and fail) to recreate BEAM files that are already correct as they
# are.


# Even if copying the relevant header/source/BEAM files with relevant
# timestamps, in some cases, for some reason, rebar will still find it relevant
# to try to rebuild some of them. So we have to hide the corresponding sources
# as well...

project_name="$1"

# Not verbose by default (1):
verbose=1
#verbose=0

if [ -n "$2" ]; then
	verbose="$2"
fi


if [ -z "${project_name}" ]; then

	echo "  Error, project name not set." 1>&2

	exit 5

fi

echo "Fixing rebar pre-build for ${project_name}: building all first, from $(pwd)."

# 'tree' may not be available:
[ $verbose -eq 1 ] || tree


make -s all 2>/dev/null


# Do not fix anything by default:
fix_src=1
fix_hdr=1
fix_beam=1


# Element locations vary depending on whether this project is the main target to
# build, or a mere dependency, or in a checkout.

target_base_dir="./_build/default/lib/${project_name}"

if [ -d "${target_base_dir}" ]; then

	# We are the main application (not a dependency), so:
	echo "Detected as being a direct build (not as a dependency)."

	fix_src=0
	fix_hdr=0
	fix_beam=0

else

	#echo "(project base directory is not '${target_base_dir}')"

	target_base_dir="../${project_name}"

	if [ ! -d "${target_base_dir}" ]; then

		echo "(project based directory is not '${target_base_dir}')"

		echo "  Error, target directory not found for ${project_name}." 1>&2

		exit 10

	fi

	echo "Detected as being built as a dependency."

	# In which case the only need is to copy ebin:
	fix_beam=0

	# Actually we nevertheless need them apparently (even if they just copy a
	# file onto itself, they at least update its timestamp in the process, like
	# a touch), otherwise another silly attempt of rebuild will be done by
	# rebar3:
	#
	fix_src=0
	fix_hdr=0

fi

echo "Copying build-related elements in the '${target_base_dir}' target tree."


# Transforming a potentially nested hierarchy (tree) into a flat directory:
# (operation order allows proper timestamp ordering)
#
# Note that 'ebin' is bound to be an actual directory, yet (probably if dev_mode
# has been set to true in rebar.config), 'include', 'priv' and 'src' may be
# symlinks to their original versions (in the source tree).
#
# As they could be nested (being trees rather than flat directories), we remove
# any pre-existing target directory and replace it with a flat copy of our own.


if [ $fix_hdr -eq 0 ]; then

	target_hdr_dir="${target_base_dir}/include"

	if [ -L "${target_hdr_dir}" ]; then

		echo "Replacing the ${target_hdr_dir} symlink by an actual directory."
		/bin/rm -f "${target_hdr_dir}"
		mkdir "${target_hdr_dir}"

	elif [ ! -d "${target_hdr_dir}" ]; then

		echo "Creating the non-existing ${target_hdr_dir} directory."
		mkdir "${target_hdr_dir}"

	else

		# This may happen when multiple attempts of build are performed:
		#echo "(${target_hdr_dir} directory already existing)"

		#echo "Unexpected target ${target_hdr_dir}: $(ls -l ${target_hdr_dir})" 1>&2
		#exit 5

		# So no-op finally:
		:

	fi

	all_hdrs=$(find src test include -name '*.hrl' 2>/dev/null)

	if [ $verbose -eq 0 ]; then
		echo "  Copying all headers to ${target_hdr_dir}: ${all_hdrs}"
	else
		echo "  Copying all headers to ${target_hdr_dir}"
	fi

	for f in ${all_hdrs}; do
		# We do not care if it is a copy of a file onto itself, a touch-like
		# operation is anyway strictly needed:
		#
		/bin/cp -f $f ${target_hdr_dir} 2>/dev/null
	done

else

	echo "(not fixing headers)"

fi



if [ $fix_src -eq 0 ]; then

	target_src_dir="${target_base_dir}/src"

	if [ -L "${target_src_dir}" ]; then

		echo "Replacing the ${target_src_dir} symlink by an actual directory."
		/bin/rm -f "${target_src_dir}"
		mkdir "${target_src_dir}"

	elif [ ! -d "${target_src_dir}" ]; then

		echo "Creating the non-existing ${target_src_dir} directory."
		mkdir "${target_src_dir}"

	fi

	all_srcs=$(find src test -name '*.erl' 2>/dev/null)

	if [ $verbose -eq 0 ]; then
		echo "  Copying all sources to ${target_src_dir} then hiding the original ones: ${all_srcs}"
	else
		echo "  Copying all sources to ${target_src_dir} then hiding the original ones"
	fi

	for f in ${all_srcs}; do
		# We do not care if it is a copy of a file onto itself, a touch-like
		# operation is anyway strictly needed:
		#
		/bin/cp -f $f ${target_src_dir} 2>/dev/null
		/bin/mv -f $f $f-hidden
	done

else

	echo "(not fixing sources)"

fi


if [ $fix_beam -eq 0 ]; then

	target_beam_dir="${target_base_dir}/ebin"

	if [ -L "${target_beam_dir}" ]; then

		echo "Replacing the ${target_beam_dir} symlink by an actual directory."
		/bin/rm -f "${target_beam_dir}"
		mkdir "${target_beam_dir}"

	elif [ ! -d "${target_beam_dir}" ]; then

		echo "Creating the non-existing ${target_beam_dir} directory."
		mkdir "${target_beam_dir}"

	fi

	all_beams=$(find src test -name '*.beam' 2>/dev/null)

	if [ $verbose -eq 0 ]; then
		echo "  Copying all BEAM files to ${target_beam_dir}: ${all_beams}"
	else
		echo "  Copying all BEAM files to ${target_beam_dir}"
	fi

	for f in ${all_beams}; do
		/bin/cp -f $f ${target_beam_dir}
	done

else

	echo "(not fixing BEAM files)"

fi


[ $verbose -eq 1 ] || (echo "Final content for ${project_name} from $(pwd):" ; tree ${target_base_dir})


echo "Rebar pre-build fixed for ${project_name}."
