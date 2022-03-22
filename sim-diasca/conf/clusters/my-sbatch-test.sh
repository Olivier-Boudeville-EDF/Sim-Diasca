#!/bin/sh

#SBATCH --job-name=my-test
#SBATCH -N 1
#SBATCH -n 1
#SBATCH --qos=test
#SBATCH -p cn
#SBATCH --exclusive
#SBATCH --time=00:02:00
#SBATCH --output=my-test-job-%j.log
#SBATCH --error=my-test-job-%j.error

# (we applied above our preferred conventions)

# Note that anyway these SBATCH options may be overridden from the command-line.
#
# For example our job-launcher.sh script may override --job-name, and will
# override --output and --error.


srun echo "Hello from $(id) at $(date), from $(pwd) on $(hostname -f)."
echo "This is some other non-srun command. End of sbatch script."
