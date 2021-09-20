# This file is an helper used by the fix-rebar-compile-{pre,post}-hook.sh
# scripts in order to define common functions once.
#
# As such it is not executable, as it is only meant to be sourced.

tree_opts="-anh"


build_target_role=1
normal_dependency_role=2

# Not used, at least currently:
checkout_role=3


#echo "Helper arguments: $*"


# Automatic setting of options.


project_name="$1"

if [ -z "${project_name}" ]; then

	echo "  Error, project name not set." 1>&2

	exit 5

fi

shift


# Hiding enabled by default (0):
do_hide=0
#do_hide=1

# Not verbose by default (1):
verbose=1
#verbose=0

if [ -n "$1" ]; then

	if [ "$1" = "--hiding-for-rebar" ]; then
		do_hide=0
	elif [ "$1" = "--no-hiding-for-rebar" ]; then
		do_hide=1
	else
		echo "  Error, invalid hiding option specified ('$1')." 1>&2
		exit 10
	fi

	shift

	if [ -n "$1" ]; then

		if [ "$1" = "--verbose" ]; then
			echo "(verbose mode activated)"
			verbose=0
		elif [ "$1" = "--no-verbose" ]; then
			verbose=1
		else
			echo "  Error, invalid verbosity option specified ('$1')." 1>&2
			exit 15
		fi

	fi

fi


if [ $do_hide -eq 0 ]; then

	echo "(hiding of erl/hrl files will be performed to prevent spurious rebuilds by rebar)"

elif [ $do_hide -eq 1 ]; then

	echo "(no hiding of erl/hrl files will be performed to prevent spurious rebuilds by rebar)"

fi


# Sets following variables depending on context, project being either the actual
# build target or just a (direct or not) dependency thereof:
#
# - target_base_dir: the target base project directory where elements (notably
# BEAM files) shall be produced
#
# - role: tells whether this project is the actual build target, a normal
#  dependency or a checkout
#
determine_build_context()
{

	# Element locations vary depending on whether this project is the actual
	# target to build, or a mere dependency, or in a checkout.

	target_base_dir="$(pwd)/_build/default/lib/${project_name}"

	if [ -d "${target_base_dir}" ]; then

		echo "Project ${project_name} is detected as being the direct actual build target (not a dependency or a checkout), as '${target_base_dir}' exists."

		role=${build_target_role}

	else

		echo "(base directory of project ${project_name} is not '${target_base_dir}')"

		target_base_dir="$(realpath $(pwd)/../${project_name})"

		if [ ! -d "${target_base_dir}" ]; then

			echo "(base directory of project ${project_name} is not '${target_base_dir}' either)"

			echo "  Error, target directory not found for ${project_name}." 1>&2

			exit 10

		fi

		echo "Project ${project_name} is detected as being built as a dependency (in ${target_base_dir})."

		role=${normal_dependency_role}

	fi

}
