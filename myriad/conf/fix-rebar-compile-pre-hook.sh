#!/bin/sh

usage="Usage: $(basename $0) PROJECT_NAME [--hiding-for-rebar|--no-hiding-for-rebar] [--verbose|--no-verbose]"

# Script defined for convenience and reliability.

# Recreates a proper rebar3 landscape, based on our build, so that rebar3 will
# not attempt (and fail) to recreate BEAM files that are already correct as they
# are.

# Even if copying the right header/source/BEAM files with relevant timestamps,
# in some cases, for some reason, rebar will still find it appropriate to try to
# rebuild at least some of them (ex: if a given project is a dependency common
# to two other prerequisites), and when rebar does that it does pass the
# relevant options, leading to a failing build. So the goal is to prevent rebar
# from rebuilding anything.

# Hiding elements, i.e. erl/hrl files (most probably that hiding BEAM files is a
# bad idea in all cases), could be a solution, as it fixes some builds (some
# spurious rebuilds being avoided), however it makes other correct ones
# incorrect (unexpected rebuilds are triggered, leading to compilation failures
# because of X.beam files from a dependency being found not having their X.erl
# counterpart due to hiding).
#
# So now hiding shall be selected on a per-project basis. See HIDING_OPT in
# GNUmakevars.inc for that.

#echo "Script arguments: $*"

helper_script="$(dirname $0)/fix-rebar-hook-helper.sh"

if [ ! -f "${helper_script}" ]; then

	echo "  Error, helper script ('${helper_script}') not found." 1>&2

	exit 8

fi


# Sets options:
. "${helper_script}"


echo
echo "Fixing rebar pre-build for ${project_name}"


# Sets following variables depending on context, project being either the actual
# build target or just a (direct or not) dependency thereof:
#
# - target_base_dir: the target base project directory where elements (notably
# BEAM files) shall be produced
#
# - role: tells whether this project is the actual build target, a normal
#  dependency or a checkout
#
determine_build_context


echo "  For ${project_name}, building all first, from $(pwd) (role: ${role}):"

# 'tree' may not be available:
if [ $verbose -eq 0 ]; then

	make -s info-context

	#echo "Pre-'make all' tree from $(pwd): "
	#tree ${tree_opts}

	if [ ${role} -eq ${normal_dependency_role} ]; then

		#echo "Content of sibling ebin directories: $(/bin/ls ../*/ebin/*)"
		dep_base_dir="$(realpath ..)"
		echo "Full content found from the root of all sibling dependencies, ${dep_base_dir}, prior to the build of ${project_name}: "
		tree ${tree_opts} "${dep_base_dir}"

	fi

fi


if ! make -s all; then

	echo "  Error, build of ${project_name} failed." 1>&2

	make -s info

	exit 10

fi


# We used not to fix anything by default; now we do the opposite, and in all
# cases (for all build roles: as the build target or as a dependency):


# Actually needed apparently (even if they just copy a file onto itself, they at
# least update its timestamp in the process, like a touch), otherwise another
# unwise attempt of rebuild will be done by rebar3:
#
fix_sources=0

fix_headers=0

# To copy ebin content:
fix_beams=0



echo "  Securing relevant build-related elements in the '${target_base_dir}' target tree."


# Transforming a potentially nested hierarchy (tree) into a flat directory:
# (operation order matters, as it allows proper timestamp ordering for make)
#
# Note that 'ebin' is bound to be an actual directory, yet (probably if dev_mode
# has been set to true in rebar.config), 'include', 'priv' and 'src' may be
# symlinks to their original versions (in the source tree).
#
# As they could be nested (being trees rather than flat directories), we remove
# any pre-existing target directory and replace it with a flat copy of our own.



if [ $fix_headers -eq 0 ]; then

	echo "   Fixing first headers"

	# Either in _build tree or local:
	target_inc_dir="${target_base_dir}/include"

	if [ -L "${target_inc_dir}" ]; then

		echo "    Replacing the ${target_inc_dir} symlink by an actual directory."
		/bin/rm -f "${target_inc_dir}"
		mkdir "${target_inc_dir}"

	elif [ ! -d "${target_inc_dir}" ]; then

		echo "    Creating the non-existing ${target_inc_dir} directory."
		mkdir "${target_inc_dir}"

	else

		# This may happen when multiple attempts of builds are performed for a
		# direct-build, or when being built as a dependency:
		#
		[ $verbose -eq 1 ] || echo "(${target_inc_dir} directory already existing)"

		#echo "Warning: unexpected target ${target_inc_dir}: $(ls -l ${target_inc_dir})" 1>&2

		#exit 5

	fi

	# For a direct build, we have to fix the _build/default/lib/ tree (i.e. to
	# perform copies), but for a build-as-a-dependency, we are already located
	# in that _build target tree (ex: just "updating then timestamps", for an
	# increased safety):
	#
	if [ ${role} -eq ${build_target_role} ]; then

		# Due to symlinks to all actual headers having possibly been already
		# created in local 'include' (projects not defining nested includes have
		# already their actual headers directly in the 'include' directory), we
		# just copy *these* symlinks (i.e. their actual target) / these actual
		# headers to the target rebar include directory - not *all* headers
		# found from the local include, as this would include both versions
		# (symlink and actual one being hidden) of headers, resulting in said
		# symlinks to be dead and their copy to fail, with for example:
		#
		# /bin/cp: cannot stat 'include/lazy_hashtable.hrl': No such file or
		# directory

		# So:
		#all_headers=$(find src test include -name '*.hrl' 2>/dev/null)
		all_headers=$(/bin/ls include/*.hrl 2>/dev/null)

		if [ $verbose -eq 0 ]; then
			echo "    Copying all headers to ${target_inc_dir}: ${all_headers}"
		else
			echo "    Copying all headers to ${target_inc_dir}"
		fi

		for f in ${all_headers}; do

			# We do not care if it is a copy of a file onto itself (should not
			# be the case anymore), a touch-like operation is anyway strictly
			# needed (however a copy of a file to itself does not update its
			# timestamp).
			#
			# (copying a symlink this way copies the actual content that it
			# references - we thus end up with a regular file in target
			# directory - not a symlink)
			#
			/bin/cp -f "$f" "${target_inc_dir}/" #2>/dev/null

			# To prevent rebar from even seeing them afterwards:
			if [ $do_hide -eq 0 ]; then
				/bin/mv -f "$f" "$f-hidden-for-rebar3"
			fi

	   done

	elif [ ${role} -eq ${normal_dependency_role} ]; then

		# Here we are within the _build tree.

		# Two possibilities:
		#
		# - just touching the headers already in place in this
		# _build/[...]/include (yet touching a symlink touches the file it
		# references, not the symlink itself whose timestamp remains)
		#
		# - or copying the actual ones found blindly (however, depending on the
		# projects, some like Myriad define a nested include hierarchy with
		# symlinks referencing them from 'include', whereas others just directly
		# put all their headers directly in 'include')

		# In this bulletproof version, we prefer copying to touching, and
		# managing all cases (nested includes with symlinks / direct includes)
		# by *copying* directly any nested includes found in 'include', then
		# touching all headers directly in 'include' (whether they are new or
		# not). So:

		cd include

		# Overwrite any symlinked header found directly in 'include' by its
		# actual referenced content found in this tree:
		#
		all_nested_headers=$(find * -mindepth 1 -a -name '*.hrl')

		# Possibly overwriting symlinks, or doing nothing:
		echo "    Copying in $(pwd) all nested headers found: ${all_nested_headers}"

		for f in ${all_nested_headers}; do

			# To avoid errors like: "/bin/cp: 'maths/linear_2D.hrl' and
			# 'linear_2D.hrl' are the same file", overwrite any symlink located
			# at target path:

			symlink_at_target="$(basename $f)"

			if [ -h "${symlink_at_target}" ]; then
				echo " (removing previously-existing include symlink ${symlink_at_target} at target)"
				/bin/rm -f "${symlink_at_target}"

			fi

			/bin/cp -f "$f" .

		done

		all_direct_headers=$(/bin/ls *.hrl 2>/dev/null)
		echo "    Touching in $(pwd) all nested headers found: ${all_direct_headers}"
		for f in ${all_direct_headers}; do

			touch "$f"

		done

		cd ..

	else

		echo "(for role ${role}, nothing done with headers)"

	fi

else

	echo "   (not fixing headers)"

fi



if [ $fix_sources -eq 0 ]; then

	echo "   Fixing then sources"

	target_src_dir="${target_base_dir}/src"

	if [ -L "${target_src_dir}" ]; then

		echo "    Replacing the ${target_src_dir} symlink by an actual directory."
		/bin/rm -f "${target_src_dir}"
		mkdir "${target_src_dir}"

	elif [ ! -d "${target_src_dir}" ]; then

		echo "    Creating the non-existing ${target_src_dir} directory."
		mkdir "${target_src_dir}"

	else

		# This may happen when multiple attempts of builds are performed for a
		# direct-build, or when being built as a dependency:
		#
		[ $verbose -eq 1 ] || echo "(${target_src_dir} directory already existing)"

		#echo "Warning: unexpected target ${target_src_dir}: $(ls -l ${target_src_dir})" 1>&2

		#exit 6

	fi

	# For a direct build, we have to fix the _build/default/lib tree (i.e. to
	# perform copies), but for a build-as-a-dependency, we are already in that
	# target tree (just updating timestamps for an increased safety):

	all_srcs=$(find src test -name '*.erl' 2>/dev/null)

	if [ ${role} -eq ${build_target_role} ]; then

		if [ $verbose -eq 0 ]; then
			echo "   Copying all sources to ${target_src_dir} then hiding the original ones: ${all_srcs}"
		else
			echo "   Copying all sources to ${target_src_dir} then hiding the original ones"
		fi

		for f in ${all_srcs}; do

			# We do not care if it is a copy of a file onto itself, a touch-like
			# operation is anyway strictly needed:
			#
			/bin/cp -f "$f" "${target_src_dir}/" #2>/dev/null

			# To prevent rebar from even seeing them afterwards:
			if [ $do_hide -eq 0 ]; then
				/bin/mv -f "$f" "$f-hidden-for-rebar3"
			fi

		done

	elif [ ${role} -eq ${normal_dependency_role} ]; then

		# Preferring hiding to touching:
		#if [ $verbose -eq 0 ]; then
		#    echo "    Touching all sources from $(pwd): ${all_srcs}"
		#else
		#    echo "    Touching all sources from $(pwd)"
		#fi

		#for f in ${all_srcs}; do
		#
		#    touch "$f"
		#
		#done

		echo "   Hiding all sources from $(pwd): ${all_srcs}"

		for f in ${all_srcs}; do

			# To prevent rebar from even seeing them afterwards:
			if [ $do_hide -eq 0 ]; then
				/bin/mv -f "$f" "$f-hidden-for-rebar3"
			fi

		done

	else

		echo "(for role ${role}, nothing done with sources)"

	fi


else

	echo "   (not fixing sources)"

fi


if [ $fix_beams -eq 0 ]; then

	echo "   Fixing finally BEAMs"

	target_ebin_dir="${target_base_dir}/ebin"


	if [ -L "${target_ebin_dir}" ]; then

		echo "    Replacing the ${target_ebin_dir} symlink by an actual directory."
		/bin/rm -f "${target_ebin_dir}"
		mkdir "${target_ebin_dir}"

	elif [ ! -d "${target_ebin_dir}" ]; then

		echo "    Creating the non-existing ${target_ebin_dir} directory."
		mkdir "${target_ebin_dir}"

	else

		# This may happen when multiple attempts of build are performed:
		# (actually normal for ebin directories)

		[ $verbose -eq 1 ] || echo "(${target_ebin_dir} directory already existing)"

		#echo "Warning: unexpected target ${target_ebin_dir}: $(ls -l ${target_ebin_dir})" 1>&2

		#exit 7

	fi

	# For BEAM files, the build role has not to be specifically managed
	# (provided that target_ebin_dir is correct):

	all_beams=$(find src test -name '*.beam' 2>/dev/null)

	if [ $verbose -eq 0 ]; then
		echo "    Copying all BEAM files to ${target_ebin_dir}: ${all_beams}"
	else
		echo "    Copying all BEAM files to ${target_ebin_dir}"
	fi

	# target_ebin_dir always right here, always a copy (not a touch):

	for f in ${all_beams}; do
		/bin/cp -f "$f" "${target_ebin_dir}/" #2>/dev/null

		# To prevent rebar from even seeing them afterwards:
		#
		# (hiding BEAMs is most probably never a good idea)
		#
		#if [ $do_hide -eq 0 ]; then
		#	/bin/mv -f "$f" "$f-hidden-for-rebar3"
		# fi

	done

else

	echo "   (not fixing BEAM files)"

fi


[ $verbose -eq 1 ] || (echo "Final content for ${project_name} from $(pwd):"; tree ${tree_opts} "${target_base_dir}")


echo "Rebar pre-build fixed for ${project_name}."
