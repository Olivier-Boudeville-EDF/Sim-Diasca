#!/bin/sh


# Copyright (C) 2008-2022 EDF R&D
#
# This file is part of Sim-Diasca.
#
# Author: Olivier Boudeville (olivier.boudeville@edf.fr)


# Currently it is not a parametric launcher, but a script to prepare a series of
# runs on a cluster.


# Defaults section.

# Set to 0 to have detailed debug information or, preferably, use the --debug
# command line-option:
#
do_debug=1

be_quiet=1

# Number of hours before the simulation is killed by the job manager:
max_duration="0"


# Default path (used if no install could be inferred), often a symbolic link to
# the selected version available in the same directory:
#
default_install_root="/scratch/${USER}/Software/Sim-Diasca/Sim-Diasca-current-install"

install_root=""

case_path=""
case_options=""

node_count=1
core_count=""
qos=""
key=""
email=""
email_notifications=""
queue_name=""


initial_dir="$(pwd)"

date_prefix="$(date +%Y%m%d)"

help_short_opt="-h"
help_long_opt="--help"

quiet_short_opt="-q"
quiet_long_opt="--quiet"

debug_short_opt="-d"
debug_long_opt="--debug"

base_archive_opt="--base-archive"
base_install_opt="--base-install"

base_name_opt="--base-name"

xp_start_opt="--experiment-start"
xp_stop_opt="--experiment-stop"

node_count_opt="--node-count"

core_per_node_short_opt="--cpn"
core_per_node_long_opt="--cores-per-node"

queue_opt="--queue"
qos_opt="--qos"

key_opt="--key"

max_dur_opt="--max-duration"

email_opt="--email"


# For any rebuild needed:
execution_target=production
#execution_target=development


usage="Usage: $(basename $0) [${help_short_opt}|${help_long_opt}] [${debug_short_opt}|${debug_long_opt}] [${base_archive_opt} ARCHIVE_FILE|${base_install_opt} INSTALL_DIR] ${base_name_opt} EXPERIMENT_PREFIX ${xp_start_opt} X Y Z ${xp_stop_opt} [${node_count_opt} NODE_COUNT] [${core_per_node_short_opt}|${core_per_node_long_opt} CORE_COUNT] [${queue_opt} QUEUE_NAME] [${qos_opt} QOS] [${key_opt} KEY] [${max_dur_opt} M] [${email_opt} EMAIL_ADDRESS EVENT_SPEC] CASE_PATH [CASE_OPTIONS]

 Launches the specified set of Sim-Diasca simulation cases on a cluster (running either a PBS or a Slurm job manager) with specified resource requirements and options. Ensures first that the Sim-Diasca installation is fully built (otherwise rebuilds lacking elements based on the '${execution_target}' execution target).

Note that either ${base_archive_opt} or ${base_install_opt} must be specified.

 CASE_PATH is either an absolute path to the case to run, or a path relative to the Sim-Diasca install root.

 CASE_OPTIONS are the command-line options (if any) that will be passed verbatim to the simulation case itself.

 Options are:
   ${help_long_opt} or ${help_short_opt}: displays this message
   ${quiet_long_opt} or ${quiet_short_opt}: activates the quiet mode
   ${debug_long_opt} or ${debug_short_opt}: activates the debug mode
   ${base_archive_opt} ARCHIVE_FILE where ARCHIVE_FILE is a path to the tar.xz base archive expected to contain Sim-Diasca and possibly the case to run, in a single directory at its root
   ${base_install_opt} INSTALL_DIR where INSTALL_DIR is a path to a pre-established directory where the full sources lie
   ${base_name_opt} EXPERIMENT_PREFIX is a prefix for all the installations that will be created
   ${xp_start_opt} A B C D ${xp_stop_opt} is an enumeration of all simulations to be run
   ${node_count_opt}: specifies the number of requested computing nodes (per experiment)
   ${core_per_node_long_opt} or ${core_per_node_short_opt}: specifies a minimal number of cores to be requested on each CPU socket
   ${queue_opt}: specifies which job queue (named partition in Slurm) is to be used (cluster-specific; ex: 'parall_128')
   ${qos_opt}: specifies the quality of service to be used for that job (cluster-specific; ex: 'cn_all_short')
   ${key_opt}: specifies a resource accounting key that may be necessary in order to enable job launching (cluster-specific)
   ${max_dur_opt}: specifies the maximal duration of the launched simulation, in wall-clock time, as DD-HH:MM, i.e. number of days, hours and minutes (not depending on the number of nodes) for this simulation, or as MM, i.e. number of minutes; if no duration, or a null (0) one, is specified, the corresponding job may never be scheduled (with Slurm, squeue may report then QOSMaxWallDurationPerJobLimit) [default: ${max_duration}]
   ${email_opt}: specifies the email address to which run notifications should be sent; an event specification must be added afterwards, it may be set either to: 'none', 'begin', 'end', 'fail' or 'all', depending on the events that shall trigger the sending of an email

 Ex: "$(basename $0)" ${base_archive_opt} /home/foo/my-sim-diasca-base-archive.tar.xz ${base_name_opt} my-experiment ${xp_start_opt} 1 2 4 ${xp_stop_opt} [...] would create, in the current directory and from the specified archive, following ready-to-run builds: $date_prefix-my-experiment-1, $date_prefix-my-experiment-2 and $date_prefix-my-experiment-4".


# No debug by default:
do_debug=1


# Transforms specified path in an absolute one.
#
# If it happens to be relative, considers it is relative to the current
# directory.
#
# Resulting path is assigned to the absolute_path variable.
#
set_as_absolute_path()
{

	path=$1

	# A path is absolute iff it starts with "/"
	if [ -z $( echo $path | sed 's|^/.*||1' ) ] ; then

		# Already absolute:
		absolute_path=$path

	else

		# Absolutizing it:
		absolute_path=$(pwd)"/$path"

	fi

}


start_date=$(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S')


#echo "Usage: ${usage}"


base_archive=""
base_dir=""
base_name=""
experiments=""


saved_command_line="Command-line was: $0 $*"


while [ -n "$*" ] ; do

	token_eaten=1


	if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then

		echo "${usage}"
		exit

	fi


	if [ "$1" = "-d" ] || [ "$1" = "--debug" ] ; then

		shift
		do_debug=0

	fi

	if [ "$1" = "${base_archive_opt}" ] ; then

		shift
		base_archive="$1"
		shift
		token_eaten=0

	fi

	if [ "$1" = "${base_install_opt}" ] ; then

		shift
		base_dir="$1"
		shift
		token_eaten=0

	fi

	if [ "$1" = "${base_name_opt}" ] ; then

		shift
		base_name="$1"
		shift
		token_eaten=0

	fi

	if [ "$1" = "${xp_start_opt}" ] ; then

		shift

		while [ "$1" != "${xp_stop_opt}" ] ; do
			experiments="$experiments $1"
			shift
		done

		# For ${xp_stop_opt}:
		shift

		token_eaten=0

	fi

	if [ $token_eaten -eq 1 ] ; then

		# Can be either absolute or relative:
		case_path="$1"
		shift

		# Now we want to support case options:

		#if [ -n "$*" ] ; then

		#	echo "  Error, unexpected parameters ($*) after case path ($case_path).
		#
		#${usage}" 1>&2

		#	exit 5

		#fi

		case_options="$*"

		# Otherwise the while will continue and overwrite the previous
		# variables:
		break

	fi

done


if [ $do_debug -eq 0 ] ; then

	echo " base_archive= $base_archive"
	echo " base_dir= $base_dir"
	echo " base_name = $base_name"
	echo " experiments = $experiments"

fi


if [ -z "$base_archive" ] ; then

	if [ -z "$base_dir" ] ; then

		echo "  Error, no base archive or base install specified.

${usage}" 1>&2
		exit 10

	else

		set_as_absolute_path "$base_dir"
		base_dir="$absolute_path"

		if [ ! -d "$base_dir" ]; then
			echo "  Error, specified base install ($base_dir) does not exist.

${usage}" 1>&2
			exit 11

		fi

	fi

else

	if [ -n "$base_dir" ] ; then

			echo "  Error, base archive and base install cannot be both specified.

${usage}" 1>&2
			exit 12

	fi

	set_as_absolute_path "$base_archive"
	base_archive="$absolute_path"

	if [ ! -f "$base_archive" ] ; then

		echo "  Error, specified base archive ($base_archive) not found.

${usage}" 1>&2
		exit 15

	fi

fi


if [ -z "$base_name" ] ; then

	echo "  Error, no base name specified.

${usage}" 1>&2
	exit 20

fi

if [ -z "$experiments" ] ; then

	echo "  Error, no experiment specified.

${usage}" 1>&2
	exit 25

fi

experiment_count=$(echo $experiments | wc -w)

echo
echo "   $experiment_count base installations will be prepared:"
echo

if [ -n "$base_archive" ] ; then

	# Using a base archive here, hence a temporary extraction directory:

	echo " + creating temporary reference installation"

	tmp_dir="$(pwd)/.tmp-parametric-install"

	if [ -d "$tmp_dir" ] ; then

		echo "Warning: removing temporary installation directory '$tmp_dir'." 1>&2
		/bin/rm -rf "$tmp_dir"

	fi

	mkdir $tmp_dir

	cd $tmp_dir

	echo "  - extracting base archive $base_archive"

	tar xJf $base_archive
	if [ ! $? -eq 0 ] ; then

		echo "  Error, extraction of base archive $base_archive failed." 1>&2
		exit 30

	fi

	extracted_dir=$(/bin/ls .)

	dir_count=$( echo $extracted_dir | wc -w )

	if [ ! $dir_count -eq 1 ] ; then

		echo "  Error, apparently the archive does not contain a single entry but $dir_count of them ($extracted_dir)." 1>&2
		exit 35

	fi

	if [ ! -d "$extracted_dir" ] ; then

		echo "  Error, the single entry of the archive ($extracted_dir) is not a directory." 1>&2
		exit 40

	fi

	set_as_absolute_path "$extracted_dir"
	base_dir="$absolute_path"

	cd "$extracted_dir"

	# From now on, the base_dir variable can be used in all cases.

else

	# Using a base install here:

	# It is known to exist, directly in expected root:
	cd $base_dir

fi


# In all cases, checking we are in a right location:
if [ ! -d "mock-simulators" ] ; then

		echo "  Error, the base directory ($(pwd)) does not seem to contain Sim-Diasca sources." 1>&2
		exit 45

fi


echo "  - cleaning base installation (in '$base_dir')"
make -s clean 1>/dev/null

BUILD_MODE=production
echo "  - pre-building base installation in $BUILD_MODE mode"

BUILD_OPT="EXECUTION_TARGET=${BUILD_MODE}"
make all ${BUILD_OPT} 1>/dev/null
if [ ! $? -eq 0 ] ; then

	echo "  Error, rebuild of the Sim-Diasca installation failed." 1>&2
	exit 35

fi


# At least on clusters, we want to selectively reactivate some modules, that are
# known to send a limited number of traces, otherwise we have to deal with
# difficult black boxes:
#
UNMUTED_MODULES="./sim-diasca/src/core/src/deployment/class_DeploymentManager.erl ./sim-diasca/src/core/src/deployment/class_ComputingHostManager.erl"

ADDITIONAL_UNMUTED_MODULES="./sim-diasca/src/core/src/instance-creation/instance_loading.erl ./sim-diasca/src/core/src/instance-creation/class_LoadBalancer.erl ./sim-diasca/src/core/src/plugins/sim_diasca_plugin.erl ./sim-diasca/src/core/src/data-management/result-management/class_ResultManager.erl"

#ALL_UNMUTED_MODULES="$UNMUTED_MODULES"
ALL_UNMUTED_MODULES="$UNMUTED_MODULES $ADDITIONAL_UNMUTED_MODULES"

UNMUTE_COUNT=$(echo $ALL_UNMUTED_MODULES | wc -w)

echo "  - unmuting $UNMUTE_COUNT modules of interest"

for m in ${ALL_UNMUTED_MODULES} ; do

	if ! touch $m ; then
		echo "  Error, module to unmute '$m' not found." 1>&2
		exit 40
	fi

done


# No ${BUILD_OPT} here on purpose; of course no clean either:
#
make all 1>/dev/null

if [ ! $? -eq 0 ] ; then

	echo "  Error, rebuild of the Sim-Diasca unmuted modules failed." 1>&2
	exit 45

fi

echo " + copying reference installation to experiment ones"

prefix="${date_prefix}-${base_name}"

cd $initial_dir


for e in $experiments ; do

	exp="${prefix}-$e"

	echo "  - preparing experiment $exp"

	exp_dir="$exp"

	if [ -d "$exp_dir" ] ; then

		echo "  Error, experiment directory '$exp_dir' already exists." 1>&2
		exit 50

	fi

	cp -r $base_dir $exp_dir

	if [ ! $? -eq 0 ] ; then

		echo "  Error, copy of the reference install to $exp_dir failed." 1>&2
		exit 55

	fi

done



# Cleaning-up:

if [ $do_debug -eq 1 ] ; then

	echo " + cleaning up temporary reference installation"
	/bin/rm -rf "$tmp_dir"

fi
