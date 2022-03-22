#!/bin/sh


# Copyright (C) 2008-2022 EDF R&D
#
# This file is part of Sim-Diasca, yet is meant to be generic.
#
# Author: Olivier Boudeville (olivier.boudeville@edf.fr)


# Generic job launcher script for computations run on clusters.
#
# See also sim-diasca-launcher.sh for its specialisation to the corresponding
# launches of Sim-Diasca simulations.


# Defaults:

# Set to 0 to have detailed debug information or, preferably, use the --debug
# command line-option:
#
do_debug=1

be_quiet=1


help_short_opt="-h"
help_long_opt="--help"

quiet_short_opt="-q"
quiet_long_opt="--quiet"

debug_short_opt="-d"
debug_long_opt="--debug"

# Optional, yet convenient to generate files with such a specific prefix:
job_name_opt="--job-name"

# Default job name:
job_name="Sim-Diasca-job-launcher"


usage="Usage: $(basename $0) [${help_short_opt}|${help_long_opt}] [${quiet_short_opt}|${quiet_long_opt}] [${debug_short_opt}|${debug_long_opt}] [${job_name_opt} JOB_NAME] EXECUTABLE [PARAMETERS]
Launches the specified executable (typically a cluster script) with the specified parameters on the current cluster (being supposedly connected to one of its frontend nodes), through either a PBS-based job manager or a Slurm one.

Example of use:
 $ $(basename $0) -q my-sbatch-test.sh
Job 18731749 submitted on Friday, March 11, 2022, at 14:00:30, waiting until it is scheduled and run...
Job queued on Friday, March 11, 2022, at 14:00:30, waiting for launch...
Submitted job (name: my-test, id: 18731749) reported as successfully completed.
Job stopped on Friday, March 11, 2022, at 14:00:31.

##### Output of job my-test (ID: 18731749) is (in /home/E21850/slurm-18731749.out):
Hello from uid=xxx(xxx) gid=xxx(xxx) groups=xxx(xxx) [...] at Fri Mar 11 14:00:30 CET 2022, on /home/xxx on xxxcn0569.foo.hpc.bar.org.
Some other non-srun command

#### No error (full output available in /home/xxx/slurm-18731749.out).

Job finished at Friday, March 11, 2022, at 14:00:31.
"

# Note: this script interacts with the job manager, and is not specific to
# Sim-Diasca per se.

# See the 'my-sbatch-test.sh' as a full example.


# This script typically outputs on the console something like (with a
# PBS-compliant job manager):

# Job identifier is 1806204, its state is:
#   Job_Name  = Sim-Diasca
#   Job_Owner = boudevil@cla11pfr
#   job_state = Q
#   queue = parall_8
# Job queued on Thursday, October 21, 2010, at 10:51:41, waiting until it is
# scheduled and run...

# And produces for example following files in
# /scratch/$USER/Software/Sim-Diasca/Sim-Diasca-current-install:
#  - Sim-Diasca.e1806204
#  - Sim-Diasca.e1806204

# A temporary file (ex: /tmp/.sim-diasca-qsub-10733.txt) is also created, it
# just contains the corresponding job ID and server node.


# With Slurm we have a typical output like:

# Using specified and validated install root directory
#  /scratch/${MY_USER}/Software/Sim-Diasca/Sim-Diasca-current-install.
# Started on Wednesday, February 4, 2015, at 17:03:07.
#  Rebuilding selectively Sim-Diasca in production mode...
# (not performing any initial clean-up)
#  Unmuting modules of interest
#  Generating the cluster script...
#  Submitting it to the job manager (slurm)...

# Job 2405075 submitted on Wednesday, February 4, 2015, at 17:03:16, waiting
# until it is scheduled and run...

# Job queued on Wednesday, February 4, 2015, at 17:03:16, waiting for launch...

# Job started on Wednesday, February 4, 2015, at 17:03:19, waiting for
# completion...



# Detects the job manager interface to be used.
#
# This function sets the system_type variable either to "slurm" or to "pbs", and
# respectively sets sbatch/qsub.
#
# (verbatim copy from the same function defined in sim-diasca-launcher.sh)
#
detect_job_manager()
{

	# We test first specifically for SLURM (via 'sbatch'), as SLURM installs
	# also a (pseudo) 'qsub', whose presence is thus not conclusive:

	system_type="undefined"

	sbatch_cmd="$(which sbatch 2>/dev/null)"

	if [ -x "${sbatch_cmd}" ]; then

		[ $be_quiet -eq 0 ] || echo "sbatch found, hence using Slurm."
		system_type="slurm"

	else

		[ $be_quiet -eq 0 ] || echo "No sbatch found hence not Slurm, testing for PBS compliance."

		qsub_cmd="$(which qsub 2>/dev/null)"

		if [ -x "${qsub_cmd}" ]; then

			[ $be_quiet -eq 0 ] || echo "qsub found, hence using PBS."
			system_type="pbs"

		else

			echo "  Error, no job manager support detected (neither Slurm nor PBS). Is this execution done on a cluster?" 1>&2
			exit 35

		fi

	fi

}


if [ "$1" = "${help_short_opt}" ] || [ "$1" = "${help_long_opt}" ]; then

	echo "${usage}"

	exit

fi


all_parameters="$*"

if [ -z "${all_parameters}" ]; then

	echo "Error, the cluster script to submit must be specified.

${usage}" 1>&2
	exit 10

fi


is_over=1

while [ $is_over -eq 1 ]; do

	token_eaten=1

	#echo "Examining $1"

	if [ "$1" = "${quiet_short_opt}" ] || [ "$1" = "${quiet_long_opt}" ]; then

		be_quiet=0
		shift
		token_eaten=0

	fi


	if  [ "$1" = "${debug_short_opt}" ] || [ "$1" = "${debug_long_opt}" ]; then

		echo "(setting debug mode)"
		do_debug=0
		shift
		token_eaten=0

	fi


	if  [ "$1" = "${job_name_opt}" ]; then

		shift
		job_name="$1"
		echo "(using job name '${job_name}')"
		shift
		token_eaten=0

	fi


	if [ $token_eaten -eq 1 ]; then

		all_parameters="$*"
		is_over=0

	fi

done


# First we select which tool for job submission should be used:

detect_job_manager

exec="$1"
[ $be_quiet -eq 0 ] || echo "Requesting the execution of the '${exec}' executable..."


# Submission-related output file.
#
# The difficulty here is to retrieve *both* the error code and the standard
# output:
#
subm_output_file="/tmp/.${job_name}-submission-by-${USER}-on-$(date '+%Y%m%d-%Hh%Mm%Ss')-$$.txt"

normal_extension="log"
error_extension="error"

if [ "${system_type}" = "pbs" ]; then

	sub_command="${qsub_cmd}"

elif [ "${system_type}" = "slurm" ]; then

	sub_command="${sbatch_cmd}"

	base_filename="${job_name}-job-%j"

	normal_file_spec="${base_filename}.${normal_extension}"
	error_file_spec="${base_filename}.${error_extension}"

	# As we do not want errors to be merged in the standard output stream:
	# (moreover we set the base of these filenames so that we know which to wait
	# for)
	#
	extra_params="--job-name=${job_name} --output=${normal_file_spec} --error=${error_file_spec}"

fi

[ $do_debug -eq 1 ] || echo "Submission command: ${sub_command} ${extra_params} ${all_parameters} 1>${subm_output_file} 2>&1"

"${sub_command}" ${extra_params} ${all_parameters} 1>"${subm_output_file}" 2>&1
res=$?

if [ ! ${res} -eq 0 ]; then

	echo
	echo "Error, job submission with ${system_type} failed (error code: ${res}).
Error message was:" 1>&2
	/bin/cat "${subm_output_file}" 1>&2
	echo
	exit 15

fi


if [ "${system_type}" = "pbs" ]; then

	# normal_file might contain an entry like '1806204.cla11pno'.
	# job_id would be then 1806204:
	#job_id=$(/bin/cat "${normal_file}" | sed 's|\..*$||1')

	# Apparently now specifying only the job ID (ex: 2891949 instead of
	# 2891949.cla11pno) will not work anymore, 'qstat -f 2891949' will indeed
	# return 'qstat: Unknown Job Id 2891949.cla13pno'. Specifying the full entry
	# read from file (ex: 2891949.cla11pno) will however work:
	#
	job_id="$(/bin/cat ${subm_output_file})"

	echo "Job identifier is ${job_id}, its state is:"

	# Full listing of the job information:
	state="$(qstat -f ${job_id})"

	job_name="$(echo "${state}" | /bin/grep Job_Name | awk '{printf $3}')"

	echo "    Job_Name  = ${job_name}"
	echo "${state}" | /bin/grep Job_Owner
	echo "${state}" | /bin/grep job_state
	echo "${state}" | /bin/grep queue

elif [ "${system_type}" = "slurm" ]; then

	# We have all the information we need as environment variables set by Slurm
	# in the environment of the launched script - not in this environment:

	#echo "Slurm_JOB_ID=${Slurm_JOB_ID}"
	#echo "Slurm_JOB_NAME=${Slurm_JOB_NAME}"
	#echo "Slurm_JOB_NODELIST=${Slurm_JOB_NODELIST}"
	#echo "Slurm_JOB_NUM_NODES=${Slurm_JOB_NUM_NODES}"
	#echo "Slurm_SUBMIT_DIR=${Slurm_SUBMIT_DIR}"

	#job_id="${Slurm_JOB_ID}"

	if [ ! -f "${subm_output_file}" ]; then

		echo "  Error, no submission output file found ('${subm_output_file}')." 1>&2
		exit 34

	fi

	job_id="$(/bin/cat ${subm_output_file} | sed 's|^Submitted batch job ||1')"

	job_name="$(scontrol show jobid ${job_id} | /bin/grep ' JobName=' | sed 's|^.* JobName=||1')"

fi


queue_time="$(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S')"

echo "Job ${job_id} submitted on ${queue_time}, waiting until it is scheduled and run..."

if [ "${system_type}" = "pbs" ]; then

	normal_file="${job_name}.o${job_id}"
	error_file="${job_name}.e${job_id}"

elif [ "${system_type}" = "slurm" ]; then

	normal_file="${job_name}-job-${job_id}.${normal_extension}"

	# With our enforced conventions:
	error_file="${job_name}-job-${job_id}.${error_extension}"

fi


[ $be_quiet -eq 0 ] || echo "Normal output file: '${normal_file}'; error one: '${error_file}'."


# Table of possible job status values (loosely listed by order of appearance):
#
# Q: Queued (pending)
# A: cAncelled
# R: Running
# F: Failed
# I: Completing
# C: Completed


running=1
spotted_as_queued=1
failed_lookups=0
max_failed_lookups=30

while [ $running -eq 1 ]; do

	if [ "${system_type}" = "pbs" ]; then

		# Either Q (queued) or R (running):
		job_status="$(qstat | /bin/grep ${job_id} | awk '{print $5}')"

	elif [ "${system_type}" = "slurm" ]; then

		job_state="$(scontrol show jobid ${job_id} | /bin/grep JobState | sed 's| Reason.*$||1' | sed 's|^   JobState=||1')"

		case ${job_state} in

			"RUNNING")
				job_status="R"
				;;

			"PENDING")
				job_status="Q"
				;;

			"FAILED")
				job_status="F"
				;;

			"CANCELLED")
				job_status="A"
				;;

			"COMPLETING")
				job_status="I"
				;;

			"COMPLETED")
				job_status="C"
				;;

			*)
				echo "Error, unexpected job state for ${job_id}: ${job_state}"
				exit 15
			  ;;

		esac

	fi

	if [ $be_quiet -eq 1 ]; then

		echo "  - job_status = ${job_status}"
		echo "  - spotted_as_queued = ${spotted_as_queued}"
		echo "  - failed_lookups = ${failed_lookups}"
		echo "  - max_failed_lookups = ${max_failed_lookups}"

	fi

	if [ "${job_status}" = "R" ]; then

		start_time="$(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S')"
		echo "Job started on $start_time, waiting for completion..."
		running=0

	elif [ "${job_status}" = "F" ]; then

		echo "Error, submitted job failed (name: ${job_name}, id: ${job_id})." 1>&2
		exit 56

	elif [ "${job_status}" = "A" ]; then

		echo "Error, submitted job was cancelled - abnormal (name: ${job_name}, id: ${job_id})." 1>&2
		exit 57

	elif [ "${job_status}" = "I" ]; then

		echo "Submitted job (name: ${job_name}, id: ${job_id}) reported as completing." 1>&2
		running=0

	elif [ "${job_status}" = "C" ]; then

		echo "Submitted job (name: ${job_name}, id: ${job_id}) reported as successfully completed." 1>&2
		running=0

	elif [ "${job_status}" = "Q" ]; then

		# Queued:
		if [ ${spotted_as_queued} -eq 1 ]; then

			queue_time="$(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S')"
			echo "Job queued on ${queue_time}, waiting for launch..."
			spotted_as_queued=0

			# Should it disappear again, this should mean it has run:
			failed_lookups=0
			max_failed_lookups=0

		fi

		sleep 1

	else

		if [ -z "${job_status}" ]; then

			if [ ${spotted_as_queued} -eq 0 ]; then

				echo "   (job named '${job_name}' with id ${job_id} not found anymore once queued, supposing it was run and completed in the meantime)"

				running=0

			else

				if [ ${failed_lookups} -eq ${max_failed_lookups} ]; then

					echo "Error, submitted job (name: ${job_name}, id: ${job_id}) not found even after ${failed_lookups} look-ups." 1>&2
					echo "Maybe the job run for too short for that script to spot it?" 1>&2

					# We suppose that the job was recorded instantaneously (no
					# race condition between the submission and the query) but
					# finished without having being spotted by this script.
					#
					# Thus there is not point in waiting for a job that most
					# probably already run (was either very short or crash at
					# start-up).
					#
					exit 20

				else

					let failed_lookups="failed_lookups+1"

					# So that the next look-ups have more chance to succeed,
					# should there be a race condition before qsub submission
					# and qstat update.
					#
					echo "   (look-up #${failed_lookups} failed: job named '${job_name}' with id ${job_id} not found yet)"
					sleep 10

				fi

			fi

		else

			echo "Error, unexpected status (${job_status}) for job (name: ${job_name}, id: ${job_id}), aborting." 1>&2

			exit 55

		fi

	fi

done


# PBS will create the output and error files when the job is finished, while
# Slurm may create them immediately and write them over time.


if [ "${system_type}" = "pbs" ]; then

	[ $be_quiet -eq 0 ] || echo "Waiting for output file '${normal_file}' in $(pwd)..."

	while [ ! -f "${normal_file}" ]; do
		[ $be_quiet -eq 0 ] || echo "  (waiting for ${normal_file}...)"
		sleep 1;

	done

elif [ "${system_type}" = "slurm" ]; then

	# Possibly never set as running:
	[ $be_quiet -eq 0 ] || echo "Waiting until job ${job_id} is not reported as running..."

	while true; do

		job_state="$(scontrol show jobid ${job_id} | /bin/grep JobState | sed 's| Reason.*$||1' | sed 's|^   JobState=||1')"

		#[ $be_quiet -eq 0 ] || echo "Job ${job_id} in state ${job_state}"

		if [ ! "${job_state}" = "RUNNING" ]; then
			break
		fi

	done

	# The writing is not instantaneous either:
	[ $be_quiet -eq 0 ] || echo "Waiting for output file '${normal_file}' in $(pwd)..."

	while [ ! -f "${normal_file}" ]; do
		[ $be_quiet -eq 0 ] || echo "  (waiting for '${normal_file}'...)"
		sleep 1;

	done

fi


stop_time="$(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S')"
echo "Job stopped on $stop_time."
echo

echo "##### Output of job ${job_name} (ID: ${job_id}) is (in $(pwd)/${normal_file}):"
/bin/cat "${normal_file}"
echo

[ $be_quiet -eq 0 ] || echo "Waiting for error file '${error_file}' in $(pwd)..."

while [ ! -f "${error_file}" ]; do
	[ $be_quiet -eq 0 ] || echo "  (waiting for '${error_file}'...)"
	sleep 1 ;
done

error_res="$(/bin/cat ${error_file})"

if [ -z "${error_res}" ]; then

  # Useful reminder if there were a lot of output:
  echo "#### No '${error_file}' error file found (full normal output available in $(pwd)/${normal_file})."

else

  echo "##### Error output for job ${job_name} (ID: ${job_id}) is (in $(pwd)/${error_file}):"

  # Strange output:
  #echo "${error_res}"

  /bin/cat "${error_file}"

  echo "Job failed at $(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S')"

  exit 50

fi

echo

if [ $do_debug -eq 1 ]; then

	/bin/rm -f "${subm_output_file}"

else

	echo "Command file left in '${subm_output_file}'."

fi


echo "Job finished at $(LC_ALL= LANG= date '+%A, %B %-e, %Y, at %k:%M:%S')."
