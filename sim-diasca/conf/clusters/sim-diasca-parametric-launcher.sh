#!/bin/sh

# Currently it is not a parametric launcher, but a script to prepare series of
# runs.


initial_dir=$(pwd)

date_prefix=$(date +%Y%m%d)


USAGE="Usage: "$(basename $0)" [ -h | --help ] [ -d | --debug ] ( --base-archive ARCHIVE_FILE | --base-install INSTALL_DIR ) --base-name EXPERIMENT_PREFIX --experiment-start X Y Z --experiment-stop [ --node-count NODE_COUNT ] [ {--cores-per-node,--cpn} CORE_COUNT ] [ --queue QUEUE_NAME ] [ --qos QOS ] [ --key KEY ] [ --max-duration M ] [ --mail MAIL_ADDRESS EVENT_SPËCIFICATION ] CASE_PATH [CASE_OPTIONS]
 Launches specified set of Sim-Diasca simulation cases on a cluster (running either a PBS or a SLURM job manager) with specified resource requirements and options. Ensures first that the Sim-Diasca installation is fully built.

Note that either --base-archive or --base-install must be specified.

 Options are:

 --base-archive ARCHIVE_FILE where ARCHIVE_FILE is a path to the tar.xz base archive expected to contain Sim-Diasca and possibly the case to run, in a single directory at its root

 --base-install INSTALL_DIR where INSTALL_DIR is a path to a pre-established directory where the full sources lie

  --base-name EXPERIMENT_PREFIX is a prefix for all the installations that will be created

  --experiment-start A B C D --experiment-stop is an enumeration of all simulations to be run

 See 'sim-diasca-launcher.sh --help' for the description of all following options.

 Ex: "$(basename $0)" --base-archive /home/foo/my-sim-diasca-base-archive.tar.xz --base-name my-experiment --experiment-start 1 2 4 --experiment-stop [...] would create in the current directory from specified archive following ready-to-run builds: $date_prefix-my-experiment-1, $date_prefix-my-experiment-2 and $date_prefix-my-experiment-4".


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


#echo "Usage: $USAGE"


base_archive=""
base_dir=""
base_name=""
experiments=""


saved_command_line="Command-line was: $0 $*"


while [ -n "$*" ] ; do

	token_eaten=1


	if [ "$1" = "-h" ] || [ "$1" = "--help" ] ; then

		echo "$USAGE"
		exit

	fi


	if [ "$1" = "-d" ] || [ "$1" = "--debug" ] ; then

		shift
		do_debug=0

	fi

	if [ "$1" = "--base-archive" ] ; then

		shift
		base_archive="$1"
		shift
		token_eaten=0

	fi

	if [ "$1" = "--base-install" ] ; then

		shift
		base_dir="$1"
		shift
		token_eaten=0

	fi

	if [ "$1" = "--base-name" ] ; then

		shift
		base_name="$1"
		shift
		token_eaten=0

	fi

	if [ "$1" = "--experiment-start" ] ; then

		shift

		while [ "$1" != "--experiment-stop" ] ; do
			experiments="$experiments $1"
			shift
		done

		# For --experiment-stop:
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
		#$USAGE" 1>&2

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

$USAGE" 1>&2
		exit 10

	else

		set_as_absolute_path "$base_dir"
		base_dir="$absolute_path"

		if [ ! -d "$base_dir" ]; then
			echo "  Error, specified base install ($base_dir) does not exist.

$USAGE" 1>&2
			exit 11

		fi

	fi

else

	if [ -n "$base_dir" ] ; then

			echo "  Error, base archive and base install cannot be both specified.

$USAGE" 1>&2
			exit 12

	fi

	set_as_absolute_path "$base_archive"
	base_archive="$absolute_path"

	if [ ! -f "$base_archive" ] ; then

		echo "  Error, specified base archive ($base_archive) not found.

$USAGE" 1>&2
		exit 15

	fi

fi


if [ -z "$base_name" ] ; then

	echo "  Error, no base name specified.

$USAGE" 1>&2
	exit 20

fi

if [ -z "$experiments" ] ; then

	echo "  Error, no experiment specified.

$USAGE" 1>&2
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
