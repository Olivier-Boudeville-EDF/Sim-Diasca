#!/bin/sh

user_name="${USER}"

echo "Killing all jobs belonging to ${user_name}..."


be_quiet=0


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


detect_job_manager


if [ "${system_type}" = "pbs" ]; then

	for j in $(qstat | grep ${user_name} | awk '{print $1}'| sed 's|\..*||1'); do

		echo "  + killing job $j"
		qdel $j

	done

	echo "...done"

elif [ "${system_type}" = "slurm" ]; then

	echo "Initial queue:"
	squeue -u "${user_name}"
	echo

	for j in $(squeue --noheader --format="%A"); do

		echo "  + killing job $j"
		scancel $j

	done

	echo "...done"

	echo
	echo "New queue, expected to be empty, is:"

	# Otherwise not applied yet:
	sleep 1

	squeue -u "${user_name}"

fi
