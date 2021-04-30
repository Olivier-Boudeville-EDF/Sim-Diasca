#!/bin/sh

# Copyright (C) 2008-2021 Olivier Boudeville
#
# Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
#
# This file is part of the Ceylan-Myriad project.



# Implementation notes:

# For daemon use:

# Previously the specified code was run with 'erl -eval [...]'. This was simple,
# however none of the execution facilities offered by the 'init' module ("-s",
# "-eval" and "-run") allows to run a VM which would resist exceptions (ex: the
# first remote node to crash would trigger a 'noconnection' exception that would
# make the launched node crash).
#
# So we allowed to switch to using run_erl, which is more heavyweight but
# restores the resistance to exceptions (ex: relying on pipes).

# Using run_erl allows to have the VM be able to resist to the crash of others;
# however then some tests may either actually fail whereas returning success, or
# may terminate in an open shell. So the default is still not using run_erl,
# unless the --daemon option is specified (in which case the log directory will
# be the one from which this script is run):

# Default is false (1):
use_run_erl=1



# Not used anymore as the user may prefer file-based cookies:
#default_cookie="ceylan_myriad"


# Some defaults:

# By default up to 1.2 million processes could be created on one node:
# (reduced, as even without having spawned these processes, the memory footprint
# can increase quite a lot; default value is 32768 processes)
# max_process_count=120000
max_process_count=400000
#max_process_count=120000000


# Often it is recommended that at least 12 threads per core are requested:
asynch_thread_count=128



usage="
Usage: $(basename $0) [-v] [-c a_cookie] [--sn a_short_node_name | --ln a_long_node_name | --nn an_ignored_node_name ] [--tcp-range min_port max_port] [--epmd-port new_port] [--fqdn a_fqdn] [--max-process-count max_count] [--busy-limit kb_size] [--async-thread-count thread_count] [--background] [--non-interactive] [--eval an_expression] [--no-auto-start] [-h|--help] [--beam-dir a_path] [--beam-paths path_1 path_2] [-start-verbatim-options [...]]: launches the Erlang interpreter with specified settings.

Detailed options:
	-v: be verbose
	-c a_cookie: specify a cookie, otherwise no cookie will be specifically set
	--sn a_short_node_name: distributed node using specified short name (ex: 'my_short_name')
	--ln a_long_node_name: distributed node using specified long name (ex: 'my_long_name')
	--nn an_ignored_node_name: non-distributed node, specified name ignored (useful to just switch the naming options)
	--tcp-range min_port max_port: specify a TCP port range for inter-node communication (useful for firewalling issues)
	--epmd-port new_port: specify a specific EPMD port (default: 4369); only relevant if the VM is to be distributed (using short or long names), initially or at runtime
	--fqdn a_fqdn: specify the FQDN to be used
	--max-process-count max_count: specify the maximum number of processes per VM (default: ${max_process_count})
	--busy-limit size: specify the distribution buffer busy limit, in kB (default: 1024)
	--async-thread-count thread_count: specify the number of asynchronous threads for driver calls (default: ${asynch_thread_count})
	--background: run the launched interpreter in the background (ideal to run as a daemon, ex: on a server)
	--daemon: run the node as a daemon (relies on run_erl and implies --background)
	--non-interactive: run the launched interpreter with no shell nor input reading (ideal to run through a job manager, ex: on a cluster)
	--eval 'an Erlang expression': start by evaluating this expression
	--no-auto-start: disable the automatic execution at VM start-up
	-h or --help: display this help
	--beam-dir a_path: adds specified directory to the path searched for beam files (multiple --beam-dir options can be specified)
	--beam-paths first_path second_path ...: adds specified directories to the path searched for beam files (multiple paths can be specified; must be the last option)
	--log-dir: specify the directory in which the VM logs (if using run_erl) shall be written

Other options will be passed 'as are' to the interpreter with a warning, except if they are listed after a '-start-verbatim-options' option, in which case they will passed with no warning.

If neither '--sn' nor '--ln' is specified, then the node will not be a distributed one.

Example: $(basename $0) -v --ln ceylan --eval 'class_TimeManager_test:run()'"


# Note that the BEAM dirs/paths are realpath'ed by this script, which may result
# some checks (ex: for the JSON backend) to wrongly conclude that some BEAM
# files can be found in more than one location in the code path.
#
# Moreover the specified paths shall preferably match the one otp_utils would
# elect, for a better homogeneity.


# Should the Erlang VM crash, the terminal (console) may not recover well (ex:
# no more echoing of the typed characters)
#
# (obtained thanks to a diff of 'stty --all' before and after the issue)
#
# See also: 'reset-keyboard-mode.sh'.
#
reset_keyboard()
{

	/bin/stty -brkint -ignpar icrnl -imaxbel opost icanon echo
	echo

}



# Uncomment to inspect input parameters:
#echo "launch-erl.sh received as parameters: $*"


cmd_file="launch-erl-input-command.sh"

# Typically this cmd_file shall be edited so that quotes are added back to the
# -eval command (ex: '-eval foobar_test:run()'):
#
#echo "$0 $*" > ${cmd_file} && chmod +x ${cmd_file} && echo "(input launch command stored in ${cmd_file})"


#erl=/usr/bin/erl
erl=$(which erl)

run_erl=$(which run_erl)
to_erl=$(which to_erl)


#CEYLAN_MYRIAD_ROOT=$(dirname $0)/../..
#echo "CEYLAN_MYRIAD_ROOT = ${CEYLAN_MYRIAD_ROOT}"


# If logs are redirected to file:
default_log_file="Ceylan-Myriad-run.log"

log_dir=$(pwd)


# Defaults:
be_verbose=1
use_tcp_range=1
autostart=0
in_background=1
non_interactive=1

# Erlang defaults (see http://erlang.org/doc/man/erl.html#+zdbbl):
busy_limit=1024



warning_prefix="[launch-erl.sh] Warning:"


# To stop interpreting options when having to manage them verbatim:
do_stop=1


while [ $# -gt 0 ] && [ $do_stop -eq 1 ]; do

	token_eaten=1

	#echo "Examining next argument: '$1'"

	if [ "$1" = "-v" ]; then
		be_verbose=0
		echo "(verbose mode activated)"
		token_eaten=0
	fi

	if [ "$1" = "-c" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no cookie specified after -c." 1>&2
			exit 12
		fi
		#echo "  + specified cookie: $cookie"
		cookie="$1"
		token_eaten=0
	fi

	if [ "$1" = "--sn" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no short name specified after --sn." 1>&2
			exit 12
		fi
		short_name="$1"
		token_eaten=0
	fi

	if [ "$1" = "--ln" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no long name specified after --ln." 1>&2
			exit 12
		fi
		long_name="$1"
		token_eaten=0
	fi

	if [ "$1" = "--nn" ]; then
		shift
		# "$1" ignored.
		token_eaten=0
	fi

	if [ "$1" = "--tcp-range" ]; then
		shift
		use_tcp_range=0
		if [ -z "$1" ]; then
			echo "  Error, no lower bound for TCP range after --tcp-range." 1>&2
			exit 12
		fi
		if [ -z "$2" ]; then
			echo "  Error, no higher bound for TCP range after --tcp-range." 1>&2
			exit 12
		fi
		lower_tcp_port="$1"
		higher_tcp_port="$2"
		shift
		# Already done at the end of the loop: shift
		#echo "  + TCP range: from $lower_tcp_port to $higher_tcp_port"
		token_eaten=0
	fi

	if [ "$1" = "--epmd-port" ]; then

		shift
		if [ -z "$1" ]; then
			echo "  Error, no EPMD port specified after --epmd-port." 1>&2
			exit 12
		fi
		epmd_port="$1"
		# This is apparently the way to notify a VM of the EPMD port, and
		# appending the epmd_port_opt before the command apparently will not
		# work ('ERL_EPMD_PORT=4269: not found'), thus exporting it instead:
		#epmd_port_opt="ERL_EPMD_PORT=$epmd_port"
		#echo "Setting EPMD port to $epmd_port"
		export ERL_EPMD_PORT="${epmd_port}"

		# This works both ways (to tell EPMD where to be launched, to tell erl
		# where to find EPMD).
		token_eaten=0

	fi

	if [ "$1" = "--fqdn" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no FQDN specified after --fqdn." 1>&2
			exit 12
		fi
		fqdn="$1"
		token_eaten=0
	fi


	if [ "$1" = "--max-process-count" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no count specified after --max-process-count." 1>&2
			exit 12
		fi
		max_process_count="$1"
		token_eaten=0
	fi


	if [ "$1" = "--busy-limit" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no busy limit specified after --busy-limit." 1>&2
			exit 12
		fi
		busy_limit="$1"
		token_eaten=0
	fi


	if [ "$1" = "--async-thread-count" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no count specified after --async-thread-count." 1>&2
			exit 12
		fi
		asynch_thread_count="$1"
		token_eaten=0
	fi


	if [ "$1" = "--background" ]; then
		in_background=0
		token_eaten=0
	fi

	if [ "$1" = "--daemon" ]; then
		#echo "(running in daemon mode)"
		use_run_erl=0
		#in_background=0
		token_eaten=0
	fi

	if [ "$1" = "--non-interactive" ]; then
		non_interactive=0
		token_eaten=0
	fi

	if [ "$1" = "--beam-paths" ]; then
		# Keep --beam-paths if first position, as will be shifted in end of loop
		while [ ! $# -eq 1 ]; do
			#echo "  + adding beam path $2"
			code_dirs="${code_dirs} $2"
			shift
		done
		token_eaten=0
	fi

	if [ "$1" = "--log-dir" ]; then
		shift
		if [ -z "$1" ]; then
			echo "  Error, no directory specified after --log-dir." 1>&2
			exit 12
		fi
		log_dir="$1"
		if [ ! -d "${log_dir}" ]; then
			echo "Creating specified yet non-existing log directory '${log_dir}'."
			# No parent created:
			mkdir "${log_dir}"
			if [ ! $? -eq 0 ]; then

				echo " Error, creating of specified log directory '${log_dir}' failed." 1>&2
				exit 15

			fi

		fi
		token_eaten=0
	fi

	if [ "$1" = "--eval" ]; then
		shift
		# We can use -s instead, which would allow to send multiple commands
		# in a row.

		if [ -z "$1" ]; then
			echo "  Error, no expression specified after --eval." 1>&2
			exit 12
		fi

		# Yes, these two versions (to_eval/to_eval_run_erl) *are* needed:

		# Not relevant for example for the automatic make rules regarding tests
		# nor if the module name contained a dash (prefer acting upon the
		# escaping on the input expression, like in "--eval
		# foobar_app:start()"):
		#
		#to_eval="-eval '$1'"

		# Found finally needed, with this form:
		to_eval="-eval $1"

		to_eval_run_erl="-eval '$1'"

		# Not used, as write pipe not used either anymore:
		eval_content="$1"

		token_eaten=0
	fi

	if [ "$1" = "--no-auto-start" ]; then
		#echo "Autostart deactivated"
		autostart=1
		token_eaten=0
	fi

	if [ "$1" = "-h" ] || [ "$1" = "--help" ]; then
		echo "${usage}"
		exit
		token_eaten=0
	fi

	if [ "$1" = "--beam-dir" ]; then
		shift
		#echo "  + adding beam dir $1"
		code_dirs="${code_dirs} $1"
		token_eaten=0
	fi

	# Rather than catching options here and putting them back just to avoid the
	# 'unknown argument' warning, consider inserting them after the
	# '-start-verbatim-options' marker and before the '-extra' one (see
	# ALL_CMD_LINE_OPTIONS for that).

	# So this form is not recommended:
	#if [ "$1" = "-XXX" ]; then
	#	verbatim_opt="${verbatim_opt} $1"
	#	token_eaten=0
	#fi


	# Ignore options that have to be interpreted by the program itself:
	if [ "$1" = "-start-verbatim-options" ]; then
		# We stop the parsing and add all remaining options remaining:
		do_stop=0
		shift

		# No difference:
		verbatim_opt="${verbatim_opt} $*"
		#verbatim_opt="${verbatim_opt} $@"

		#echo "verbatim_opt = ${verbatim_opt}"

		token_eaten=0
	fi

	if [ $token_eaten -eq 1 ]; then
		echo "${warning_prefix} unknown argument ('$1'), adding \
it 'as is' to command-line." 1>&2
		verbatim_opt="${verbatim_opt} $1"
	fi

	# Prevents an unwanted shift to be done if no verbatim option was specified:
	if [ $do_stop -eq 1 ]; then
		shift
	fi

done

# The user might have specified one (or more) '-start-verbatim-options', and
# this option is also added for internal purpose, so we remove any duplicate of
# it to keep only the actual verbatim options:
#
filtered_verbatim_opt=""

for opt in ${verbatim_opt}; do
	if [ "$opt" != "-start-verbatim-options" ]; then
		filtered_verbatim_opt="$filtered_verbatim_opt $opt"
	fi
done

verbatim_opt="${filtered_verbatim_opt}"

#echo "Verbatim options: '${verbatim_opt}'."



# The PID of this current UNIX process:
shell_pid=$$

if [ $use_run_erl -eq 0 ]; then

	# Suffixes the PID, for unicity (the / suffix does not seem relevant,
	# despite http://www.erlang.org/doc/man/run_erl.html):
	#
	run_pipe="/tmp/launch-erl-${shell_pid}"

	write_pipe="${run_pipe}.w"

	read_pipe="${run_pipe}.r"

	# sh does not have '-p':
	if [ -e "${write_pipe}" ]; then
		#echo "(removing write pipe)"
		/bin/rm -f "${write_pipe}"
	fi

	if [ -e "${read_pipe}" ]; then
		#echo "(removing read pipe)"
		/bin/rm -f "${read_pipe}"
	fi


	if [ -e "${write_pipe}" ]; then
		echo "  Error, write pipe (${write_pipe}) resisted deletion." 1>&2
		exit 50
	fi

	if [ -e "${read_pipe}" ]; then
		echo "  Error, read pipe (${read_pipe}) resisted deletion." 1>&2
		exit 55
	fi

fi



#+W w : log warnings as warnings.
#log_opt="+W w -kernel error_logger "{file,\"${default_log_file}\"}
log_opt="+W w"


# First we ensure that the epmd program will be started with relaxed command
# checking (refer to http://erlang.org/doc/man/epmd.html), otherwise we will not
# be able to specifically unregister crashed nodes (ex: from node-cleaner.sh,
# refer to its embedded comments for more details):
#
# ('export ERL_EPMD_RELAXED_COMMAND_CHECK' would not suffice, no available in
# 'env' and not seen from epmd either)
#
export ERL_EPMD_RELAXED_COMMAND_CHECK=1


# Shortening as much as possible the paths, for clarity:
realpath_exec=$(which realpath 2>/dev/null)


if [ -x "${realpath_exec}" ]; then

	shortened_code_dirs=""

	# Ideally we would use:
	#current_dir=$(pwd)
	#${realpath_exec} --relative-to=$current_dir MY_PATH
	#
	# ... however older distributions do not support that realpath option, and
	# moreover the Erlang VM may retain verbatim the paths and therefore apply
	# them wrongly if they are relative and if it switches its current
	# directory. So:

	for d in ${code_dirs}; do

		#echo "  - $d"
		#new_dir=$(realpath --relative-to=$current_dir $d)

		# Side-effect: realpath by default checks that the directory exists.
		new_dir=$(realpath $d 2>/dev/null)

		if [ -d "$new_dir" ]; then
			#echo "  + $new_dir"
			shortened_code_dirs="$shortened_code_dirs $new_dir"
		else

			# Very useful warning, yet now that we support Hex packages, we have
			# to define extra ebin directories should a dependency be accessed
			# through to a built OTP application, and such permanent warnings
			# would be unpleasant, so we disabled the following:

			# echo "${warning_prefix} directory '$d' does not exist." 1>&2

			# No-op:
			:

		fi

	done

	code_dirs="${shortened_code_dirs}"

else

	echo "(warning: 'realpath' executable not found, code directories not shortened)" 1>&2

fi


# Not using '-smp auto' anymore, as the SMP mode is needed even with a single
# core if GUI (WxWindows) is to be used:
#
# +native not used here:

if [ -z "${code_dirs}" ]; then
	code_dirs_opt=""
else
	code_dirs_opt="-pz ${code_dirs}"
fi

code_opt="${code_dirs_opt} -smp +K true +A ${asynch_thread_count} +zdbbl ${busy_limit}"


# Adding the executable last to be able to prefix options:
command="${log_opt} ${code_opt} +P ${max_process_count}"

# Adds a command-line cookie only if specified:
if [ -n "${cookie}" ]; then
	cookie_opt="-setcookie ${cookie}"
fi


if [ ${autostart} -eq 1 ]; then
	echo " ** No autostart wanted, but you can run manually: ${eval_content} **"
	to_eval="-eval io:format(\"-->"${eval_content}".\")"
fi


if [ $use_tcp_range -eq 0 ]; then

	tcp_port_opt="-kernel inet_dist_listen_min ${lower_tcp_port} inet_dist_listen_max ${higher_tcp_port}"

fi

command="${command} ${cookie_opt} ${tcp_port_opt}"



# nslookup could be used as well:
# (some laptops timeout when using the 'host' command)
#
if [ -z "${fqdn}" ]; then

	# Not used anymore:
	#fqdn=$(host $(hostname) | awk '{ print $1 }' | head -n 1)
	fqdn=$(hostname -f)
	#echo "Guessed FQDN is ${fqdn}"

fi


# By default, unless short or long naming required:
actual_name="(anonymous node)"

# Tells whether the node will be a distributed one (default: false):
is_distributed=1

if [ -n "${short_name}" ]; then

	if [ -z "${long_name}" ]; then

		command="${command} -sname ${short_name}"

		actual_name="${short_name}"

		is_distributed=0

	else

		echo "Error, --sn and --ln cannot be used simultaneously." 1>&2

		exit 1

	fi

	# Distributed, with short names here:

	if [ $be_verbose -eq 0 ]; then

		# Displayed later: echo "Launching: ${command}"
		:

	else

		# No-op:
		:

		#echo "Launching the Erlang VM with short name ${short_name}."

	fi

else

	if [ -z "${long_name}" ]; then

		# Non-distributed node here:

		if [ $be_verbose -eq 0 ]; then

			# Displayed later: echo "Launching: ${command}"
			:

		else

			# No-op:
			:

			echo "Launching the Erlang VM in non-distributed mode."

		fi

	else

		# Distributed, with long names here:

		actual_name="${long_name}"

		# Commented-out, as otherwise we will indeed avoid the "Can't set long
		# node name! Please check your configuration" blocking error, but
		# afterwards Indeed one should let the VM adds by itself the proper
		# hostname:
		#
		#long_name="${long_name}@${fqdn}"

		command="${command} -name ${long_name}"

		is_distributed=0

	fi

	# Useless (would be a duplicate)
	#if [ $be_verbose -eq 0 ]; then
	#
	#	echo "Launching: ${command}"
	#
	#else

		# No-op:
	#	:

		#echo "Launching the Erlang VM with long name ${long_name}"

	#fi

fi


# Note: in both of the next cases, having an (explicit or implied) '-noinput'
# option is likely to affect negatively the Unicode support; refer to
# io:setopts/1 and the note in file_utils:open/2 for further information.

if [ $in_background -eq 0 ]; then

	# -detached used, and implies (among other things like being a
	# non-blocking command), '-noinput -noshell':
	#
	background_opt="-detached"

fi


if [ $non_interactive -eq 0 ]; then

	# No -detached used here:
	non_interactive_opt="-noinput -noshell"

fi



# Impacts the heuristics used by the shell but also by some output functions
# (such as io:format("~ts", ...):
#
language_opt="+pc unicode"


# Note: the EPMD port cannot be set that way, apparently. It can be set thanks
# to an export, though (see ERL_EPMD_PORT above).
#
#command="${epmd_port_opt} ${command} ${background_opt} ${language_opt} ${non_interactive_opt} ${verbatim_opt}"

# We removed '--launch-erl-pid=${shell_pid}', not used anymore:
command="${command} ${background_opt} ${language_opt} ${non_interactive_opt} ${verbatim_opt}"


if [ $use_run_erl -eq 0 ]; then

	if [ ! -x "${run_erl}" ]; then

		echo "  Error, no 'run_erl' tool available." 1>&2
		exit 80

	fi

	#echo "Launching a VM, using run_erl and log_dir=$log_dir."

	# The '-daemon' option must be included (see
	# http://www.erlang.org/doc/man/run_erl.html):

	# We could not include to_eval here (i.e. no 'exec ${erl} ${to_eval}...')
	# and write it in the pipe afterwards:
	#
	final_command="${run_erl} -daemon ${run_pipe} ${log_dir} \"exec ${erl} ${to_eval_run_erl} ${command}\""

else

	if [ ! -x "${erl}" ]; then

		echo "  Error, no Erlang interpreter ('erl') available." 1>&2
		exit 81

	fi

	#echo "Launching a VM, using direct command-line execution."
	final_command="${erl} ${to_eval} ${command}"

fi



# Uncomment to see the actual runtime settings:

# Log to text file:
#echo "$0 running final command: ${final_command}, with use_run_erl = $use_run_erl" > launch-erl-command.txt

# Log to console:
#echo "$0 running final command: '${final_command}', with use_run_erl = $use_run_erl"


if [ $use_run_erl -eq 0 ]; then

	#echo "run_erl command: ${final_command}"

	if [ $be_verbose -eq 0 ]; then

		echo "Launching with run_erl: ${final_command}"

	fi

	# eval is needed for nested expansion, otherwise:
	# 'Syntax error: Unterminated quoted string'.
	#
	eval "${final_command}"

	# Disabled as not working properly:

	# We want to halt the next 'tail -f' when the interpreter stops. For that we
	# have to find its PID.
	#erl_pid=""

	# while [ -z "$erl_pid" ]; do

	# erl_pid=$(ps -edf -w -w | grep beam.smp | grep "launch-erl-pid" | awk '{print $2}')
	# ps -edf | grep beam.smp
	# echo "erl_pid = $erl_pid"

	# done

	#echo "erl_pid = $erl_pid"

else

	# Not using run_erl here, direct launch (the current default):

	#echo "direct command: ${final_command}"

	if [ $be_verbose -eq 0 ]; then

		echo "Launching (directly): ${final_command}"

	fi

	${final_command}

fi


res=$?

# However run_erl may return 0 despite errors:
if [ ! $res -eq 0 ]; then

	reset_keyboard
	echo "(command failed, with error result $res)" 1>&2
	exit $res

elif [ $use_run_erl -eq 1 ]; then

	echo "(command success reported)"

fi


#pid=$!

# Commented out, as pid never set:
#if [ $in_background -eq 0 ]; then
#	echo "(PID of launched interpreter is $pid)"
#fi


if [ $use_run_erl -eq 0 ] && [ $autostart -eq 0 ]; then

	#echo "Waiting for the creation of write pipe '${write_pipe}'."

	# Wait for creation:

	# Number of seconds before time-out:
	wait_max=60

	wait_count=0

	while [ ! -e "${write_pipe}" ]; do

		wait_remain=$(($wait_max - $wait_count))

		if [ $wait_remain -eq 0 ]; then

			echo  "Error, time-out while waiting for the creation of write pipe (${write_pipe})." 1>&2
			echo "Please check that there is no identically-named Erlang VM running in the background that would block this launch." 1>&2

			# On at least some cases, the name is never found (too long
			# command-line truncated; or the node may not be a distributed one),
			# hence this has been disabled:
			#
			#ps -edf | grep beam | grep "$actual_name" | grep -v grep >&2

			exit 25

		fi

		# Do not start displaying the count-down before 5 seconds, then only
		# every 5 seconds:
		#
		if [ $wait_count -gt 4 ] && [ $(expr $wait_count % 5) -eq 0 ]; then
			echo " (launch time-out in $wait_remain seconds)"
		fi

		sleep 1

		wait_count=$(($wait_count+1))

	done

	wait_count=0
	wait_max=2

	# If a node with the same name already exists, the write pipe will exist for
	# a brief time then will be removed:

	echo "Write pipe '${write_pipe}' found, waiting $wait_max seconds to ensure start-up is successful indeed."

	while [ $wait_count -lt $wait_max ]; do

		sleep 1

		if [ ! -e "${write_pipe}" ]; then

			echo -e "\n  Error, launch failed, write pipe disappeared. Check that a node with the same name is not already existing, or that the launched code does not crash at start-up.\n" 1>&2

			exit 55

		fi

		wait_count=$(($wait_count+1))

	done



	echo -e "\n  **************************************************************"
	echo "  ** Node '${actual_name}' ready and running as a daemon."

	echo "  ** Use 'to_erl $run_pipe' to connect to that node."
	echo "  ** (then type CTRL-D to exit without killing the node)"
	echo -e "  **************************************************************"


	# Example of what can be done afterwards than to the pipes and to_erl:

	# (now the eval information are directly passed when running run_erl)

	#echo "Evaluating: '${eval_content}.'"

	# Then send the actual command:
	#echo "${eval_content}." >> ${write_pipe}

	#if [ ! -x "${to_erl}" ]; then
	#
	#	echo "  Error, the 'to_erl' tool is not available." 1>&2
	#
	#	exit 56
	#
	#fi

	#echo "Running '${to_erl} $run_pipe' now."
	#${to_erl} $run_pipe
	#echo "(to_erl just finished)"

	# Apparently, with run_erl/to_erl, in some cases some VMs linger, though
	# (i.e. to_erl terminates whereas apparently some VMs like
	# Datalogging_test-boudevil still exist).
	#
	# This is currenly not detected. However, now, a next clashing launch will
	# be clearly advertised (at the start of the second instance, not at the
	# expected end of the first).

	# And monitors its result:

	# Clean-up:

	#if [ -e "${write_pipe}" ]; then

	#	/bin/rm -f "${write_pipe}"

	#fi


	#if [ -e "${read_pipe}" ]; then

	#	/bin/rm -f "${read_pipe}"

	#fi

fi


exit 0
