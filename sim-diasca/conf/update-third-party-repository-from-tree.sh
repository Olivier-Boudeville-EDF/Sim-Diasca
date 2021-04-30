#!/bin/sh


USAGE="

 Usage: "$(basename $0)" [SIM_DIASCA_ROOT] THIRD_PARTY_REPOSITORY_TREE

 Updates, from specified (presumably one of our developing branches) or implied (deduced from the path of this script) Sim-Diasca root, an external, third-party tree (typically a checkout'd GIT branch of a remote repository).

 Example: "$(basename $0)" $HOME/Project/Sim-Diasca/sources $HOME/Projects/Foobar/src or $(basename $0) $HOME/Projects/Foobar/src"


if [ $# -eq 1 ] ; then

	# Root implied here:

	sim_diasca_root=$( echo $(pwd) | sed 's|/sim-diasca/conf||1' )

	if [ ! -d "${sim_diasca_root}" ] ; then

		echo
		echo " Error, no Sim-Diasca root specified and cannot guess current root (tried '${sim_diasca_root}')." 1>&2
		exit 10

	fi

	echo " No Sim-Diasca root specified, using the install root deduced from this script instead, i.e. '${sim_diasca_root}'."

elif [ $# -eq 2 ] ; then

	sim_diasca_root="$1"
	shift
	if [ ! -d "$sim_diasca_root" ] ; then

		echo " Error, input Sim-Diasca root '$sim_diasca_root' is not an existing directory.$USAGE" 1>&2
		exit 15

	fi

else

	echo " Error, one or two parameters are needed.$USAGE" 1>&2
	exit 20

fi



if [ ! -d "$sim_diasca_root/wooper" ] ; then

	echo " Error, input Sim-Diasca root '$sim_diasca_root' does not look like the root of a suitable check-out (no wooper directory found)." 1>&2
	exit 25

fi


# Because of the shift:
remote_repository="$1"

if [ ! -d "$remote_repository" ] ; then

	echo "Error, third-party repository '$remote_repository' is not an existing directory." 1>&2
	exit 30

fi


if [ ! -d "$remote_repository/wooper" ] ; then

	echo " Error, third-party repository '$remote_repository' does not look like the root of a suitable tree (no wooper directory)." 1>&2
	exit 21

fi

echo
echo "  Updating target repository '$remote_repository' from Sim-Diasca root '$sim_diasca_root'..."
echo

if [ $(echo $remote_repository | head -c 1) = "/" ] ; then

	absolute_remote_repository="$remote_repository"

else

	absolute_remote_repository=$(pwd)"/$remote_repository"

fi


cd $sim_diasca_root

echo " - preparing a release from $sim_diasca_root"

make prepare-release 1>/dev/null

if [ ! $? -eq 0 ] ; then
	echo " Release preparation failed." 1>&2
	exit 50
fi

release_root=$(make info-release | grep -v SIM_DIASCA_RELEASE_BASENAME | grep SIM_DIASCA_RELEASE_BASE| sed 's|^.*= ||1')
echo " - guessing release root ($release_root)"


echo " - copying release to $remote_repository"
/bin/cp -r -f ${release_root}/* $absolute_remote_repository

if [ ! $? -eq 0 ] ; then
	echo " Release preparation failed." 1>&2
	exit 50
fi


echo " - cleaning release temporary directory"
make clean-release 1>/dev/null

date=$(LANG= date '+%A, %B %-e, %Y')

message="Sim-Diasca code update, made on $date."

echo " - one can then issue: cd $remote_repository && git add ."
echo " - one having checked for untracked files, moved ones, removed ones etc., one can issue: 'git commit -m \"$message\"' && git push"