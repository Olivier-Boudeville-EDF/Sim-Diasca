#!/bin/sh

# Copyright (C) 2009-2022 Olivier Boudeville
#
# Author: Olivier Boudeville [olivier (dot) boudeville (at) esperide (dot) com]
#
# This file is part of the Ceylan-Myriad project.


LANG=C; export LANG

# Parallel build and type checking might be quite power consuming, at the risk
# of overheat.
#
# We try to prevent that by forcing a lesser (actually: least) favorable
# scheduling for the corresponding processes; however 'nice' has little to no
# effect (it is only useful to enforce respective priorities between running
# executables), generating the PLT still leads to higher temperatures; see next
# cpulimit section.
#
#nice_opt=""
nice_opt="nice --adjustment=19"


# cpulimit allows to enforce a stricter CPU usage limit to a process.
#
# It may not be available on the local host (with Arch Linux, use 'pacman -Sy
# cpulimit').
#
# No root priviledges specifically needed.
#
cpulimit="$(which cpulimit 2>/dev/null)"

if [ -x "${cpulimit}" ]; then

	# Only 50% of one "CPU" (presumably one core - hence not a lot at all):
	#cpu_limit_percentage=50

	# Better:
	cpu_limit_percentage=100

	# For testing:
	#cpu_limit_percentage=1

	# We leave any active 'nice'.

	# Add -v for a lot more information:
	cpu_limit_expr="${cpulimit} --limit=${cpu_limit_percentage} --include-children"

fi


# Note: if one wants to download src or doc base (i.e. not patch ones) archives
# by oneself, one may also point directly to http://erlang.org/download/.

# Now we keep the SHA512 (previously: MD5) sums of the sources of former
# Erlang/OTP versions, in order to reduce any risk of downloading an altered
# source archive, and to be able to switch back and forth more easily:

# To be obtained with: 'sha512sum otp_src_x.y.tar.gz':
erlang_sha512_for_25_2="2bfa14a75f91caa6e8e11a2de871539ead8b0c44c07412a000df04f6b2ed2fc3f75ca0f345bf2c01c322afa9122dc96e5241ba1d39abb400bdadcd93446ad478"
erlang_sha512_for_25_1="26f597d3637445fa3f602e75b7854d6386f4a6e5702715f4339c5107b02a571b9ff15e6f48a81ff82d994a10d396e6154609d0d8d74076afa305c9fe87efbefa"

erlang_md5_for_25_0="3c4c9da083f2a6b89aa1766415339d0c"
erlang_md5_for_24_3="eb6dd8eaee8c97cf076eda0091abbc4c"
erlang_md5_for_24_2="ebb6e865738255ae31ff680cc96d71a9"
erlang_md5_for_24_1_5="39927334547d84ef0dc9e3a39b5c32ff"
erlang_md5_for_24_1_4="392a5faf394304f7b8fb5cde0deca582"
erlang_md5_for_24_1="e740b90a20c0f63108f879ce1f228582"
erlang_md5_for_24_0="7227024b8619e4d97a7af4f4cf98d4db"
erlang_md5_for_23_3="d6660705f01afbe3466c0a5de21ab361"
erlang_md5_for_23_2="e315f59eb9e420a0e469c09649f4303f"
erlang_md5_for_23_1="3dba61234519884664e032616a61353d"
erlang_md5_for_23_0="ab781ffd75cf4ae3ddb4ed6dc3cc31b8"
erlang_md5_for_22_2="b2b48dad6e69c1e882843edbf2abcfd3"
erlang_md5_for_22_1="c23a64fecec779fd2d07074553d4625e"
erlang_md5_for_22_0="9842663e49f48e967c44b7574423f9e9"
erlang_md5_for_21_3="a50dcad2a4344b993efec8d3aff34936"
erlang_md5_for_21_2="8a797dfe4cfb1bbf1b007f01b2f5a1ad"
erlang_md5_for_21_1="482f91cf34c2ffb1dff6e716a15afefa"
erlang_md5_for_21_0="350988f024f88e9839c3715b35e7e27a"
erlang_md5_for_20_1="4c9eb112cd0e56f17c474218825060ee"


# Current stable (an update of the next two lines is needed):
erlang_version="25.2"
erlang_sum="${erlang_sha512_for_25_2}"


# Candidate version (e.g. either cutting-edge or, most probably, the previous
# version that we deem stable enough, should the current introduce regressions):
#
erlang_version_candidate="25.1"
erlang_sum_candidate="${erlang_sha512_for_25_1}"

base_install_dir="${HOME}/Software/Erlang"


plt_file="Erlang-${erlang_version}.plt"
plt_link="Erlang.plt"


mv="/bin/mv"
tar="/bin/tar"
rm="/bin/rm"
ln="/bin/ln"
mkdir="/bin/mkdir"


help_opt_short="-h"
help_opt_long="--help"

version_opt_short="-v"
version_opt_long="--version"

doc_opt_short="-d"
doc_opt_long="--doc-install"

plt_opt_short="-g"
plt_opt_long="--generate-plt"

download_opt_short="-n"
download_opt_long="--no-download"

# Not shown anymore:
patch_opt_short="-np"
patch_opt_long="--no-patch"

limit_opt_short="-ncl"
limit_opt_long="--no-cpu-limit"

previous_opt_short="-p"
previous_opt_long="--previous"

# Currently removed:
#   [${patch_opt_short}|${patch_opt_long}]
#   downloads, patches, builds and installs
#
#   ${patch_opt_short} or ${patch_opt_long}: disable the automatic patching we
#   make use of

usage="Usage: $(basename $0) [${help_opt_short}|${help_opt_long}] [${version_opt_short}|${version_opt_long}] [${doc_opt_short}|${doc_opt_long}] [${plt_opt_short}|${plt_opt_long}] [${download_opt_short}|${download_opt_long}] [${limit_opt_short}|${limit_opt_long}] [${previous_opt_short}|${previous_opt_long}] [<base install directory>]: downloads, builds and installs a fresh ${erlang_version} Erlang version in the specified base install directory (if defined), or in default directory, and in this case adds a symbolic link pointing to it from its parent directory so that an 'Erlang-current-install' symbolic link always points to the latest installed version.

Note that, if relevant archives are found in the current directory, they will be used, even if the user did not specify a 'no download' option.

If no base install directory is specified, then:
 - if this script is run as root thanks to a sudo (i.e. 'sudo $(basename $0)...'), Erlang will be built by the (supposedly non-priviledged) original sudoer in the current directory, before being installed as root in /usr/local/ (i.e. system-wide); no Erlang-current-install symbolic link applies then
 - otherwise it will be installed in ${base_install_dir}/Erlang-${erlang_version}/.

Otherwise, i.e. if a base install directory MY_DIR is specified, then Erlang will be installed into MY_DIR/Erlang/Erlang-${erlang_version}/.

Note that unless specified otherwise the whole procedure is slowed down to avoid any overheat of the local host.

Options:
	${doc_opt_short} or ${doc_opt_long}: download and install the corresponding documentation as well
	${version_opt_short} or ${version_opt_long}: just returns the current version of Erlang that would be installed
	${plt_opt_short} or ${plt_opt_long}: generate the PLT file (${plt_file}) for Dialyzer corresponding to this Erlang/OTP install
	${download_opt_short} or ${download_opt_long}: do not attempt to download anything; expect that needed files are already available (useful if not having a direct access to the Internet)
	${limit_opt_short} or ${limit_opt_long}: do not slow down the build
	${previous_opt_short} or ${previous_opt_long}: use, instead of the current Erlang version registered for installation (i.e. ${erlang_version}), the previous one (e.g. latest supported release candidate version otherwise the lastly supported stable version), namely, currently, ${erlang_version_candidate}

Example:
  install-erlang.sh ${doc_opt_long} ${download_opt_long} ${plt_opt_long}
	will install the latest available version of Erlang, with its documentation, in the ${base_install_dir}/Erlang directory, without downloading anything,
	  - or -
  install-erlang.sh ${doc_opt_long} ~/my-directory
	will install current official stable version of Erlang (${erlang_version}), with its documentation, in the ~/my-directory/Erlang/Erlang-${erlang_version} base directory, by downloading Erlang archives from the Internet
	  - or -
  sudo install-erlang.sh
	will install current official stable version of Erlang (${erlang_version}) in /usr/local/ (i.e. system-wide)

For Debian-based distributions (Debian 10, buster for example), you should preferably run beforehand, as root: 'apt-get update && apt-get build-dep erlang && apt-get install g++ make libncurses5-dev openssl libssl-dev libwxgtk2.8-dev libgl1-mesa-dev libglu1-mesa-dev libpng16-16', otherwise for example the crypto, wx or observer modules might not be available or usable.

Note also that a recent-enough C++ compiler (typically gcc) must be available and must support C++17, which is needed for the JIT VM feature that we made mandatory.
"


# Additional notes:

# On some distributions (e.g. Arch Linux), the wx module is not available, as
# wxWidgets is not detected.
#
# The root of the problem is that no /bin/wx-config executable is found.
#
# One may have to run, as root: 'cd /bin && ln -s wx-config-2.8 wx-config' for
# example.
#
# Once Erlang is compiled, this backend can be tested with: wx:demo().
#
# Another related problem is that libtinfo.so might not be found. A solution is
# to create a symlink to libncurses, which include it:
# cd /usr/lib; ln -s libncurses.so.5 -T libtinfo.so.5
#
# For Arch display-less servers:
# pacman -S gcc make openssl


# By default, will download files:
do_download=0


# By default, will not manage the documentation:
do_manage_doc=1

# By default, will not generate the PLT file:
do_generate_plt=1

# By default, use an installation prefix:
use_prefix=0

# By default, the Erlang sources will NOT be patched to better suit our use (as
# useless since Erlang 21.0):
#
do_patch=1

# By default, enforce a limit in CPU usage:
do_cpulimit=0

# Only for base (non-patch) release:
#erlang_download_location="https://erlang.org/download"


# See list in https://github.com/erlang/otp/releases:
erlang_download_location="https://github.com/erlang/otp/releases/download/OTP-${erlang_version}"

# The user that is to perform the build (everything but installation):
build_user="$(id -un)"


# Sets the wget variable appropriately.
set_wget()
{

	if [ -z "${wget}" ]; then

		wget="$(which wget)"

		if [ ! -x "${wget}" ]; then

			echo "  Error, no wget tool found, exiting." 1>&2
			exit 10

		fi

	fi

}

wget_opts="--progress=bar"



# Read all known options:

token_eaten=0

while [ $token_eaten -eq 0 ]; do

	read_parameter="$1"
	#echo "read_parameter = ${read_parameter}"

	token_eaten=1


	if [ "$1" = "${help_opt_short}" ] || [ "$1" = "${help_opt_long}" ]; then

		echo "${usage}"
		exit

	fi

	if [ "$1" = "${version_opt_short}" ] || [ "$1" = "${version_opt_long}" ]; then

		echo "${erlang_version}"
		exit

	fi


	if [ "$1" = "${previous_opt_short}" ] || [ "$1" = "${previous_opt_long}" ]; then

		erlang_version="${erlang_version_candidate}"
		erlang_sum="${erlang_sum_candidate}"
		plt_file="Erlang-${erlang_version}_candidate"

		echo "Warning: not installing the default version of Erlang currently supported, using candidate one, i.e. version ${erlang_version}." 1>&2

		token_eaten=0

	fi


	if [ "$1" = "${doc_opt_short}" ] || [ "$1" = "${doc_opt_long}" ]; then

		echo "Will manage the corresponding documentation."
		do_manage_doc=0
		token_eaten=0

	fi


	if [ "$1" = "${plt_opt_short}" ] || [ "$1" = "${plt_opt_long}" ]; then

		echo "Will generate the PLT file ${plt_file} for Dialyzer."
		do_generate_plt=0
		token_eaten=0

	fi


	if [ "$1" = "${download_opt_short}" ] || [ "$1" = "${download_opt_long}" ]; then

		echo "No file will be downloaded."
		do_download=1
		token_eaten=0

	fi

	if [ "$1" = "${limit_opt_short}" ] || [ "$1" = "${limit_opt_long}" ]; then

		echo "Build will not be slowed down."
		do_cpulimit=1
		nice_opt=""
		cpu_limit_expr=""

		token_eaten=0

	fi


	if [ "$1" = "${patch_opt_short}" ] || [ "$1" = "${patch_opt_long}" ]; then

		echo "No patch will be applied to the Erlang sources."
		do_patch=1
		token_eaten=0

	fi

	if [ -n "${read_parameter}" ]; then
		shift
	fi

done


# We had to define that variable, as for a (non-priviledged) user U, at least on
# some settings, sudo -u U <a command> will fail ("Sorry, user U is not allowed
# to execute 'XXX' as U on H."), so now we execute sudo iff strictly necessary:
#
sudo_cmd=""


# By default, the Erlang build tree will be removed:
do_remove_build_tree=0


# Then check whether one parameter remains:

if [ -z "${read_parameter}" ]; then

	# Here no base installation directory was specified:

	if [ "$(id -u)" = "0" ]; then

		if [ -z "${SUDO_USER}" ]; then

			echo "Error, if this script is to be run as root, 'sudo' shall be used, so that build operations can be performed as a normal user (not with root priviledges)." 1>&2
			exit 55

		fi

		build_user="${SUDO_USER}"

		# Run as root, no prefix specified, thus:
		use_prefix=1

		# Thus not relevant:
		#prefix="/usr/local"

		echo "Run as sudo root, thus using default system installation directory, falling back to user '${build_user}' for the operations that permit it."

		# So here sudo is a way to decrease, not increase, priviledges:
		sudo_cmd="sudo -u ${build_user}"

	else

		prefix="${base_install_dir}/Erlang-${erlang_version}"
		echo "Not run as root, thus using default installation directory '${prefix}' (and user '${build_user}')."

		# In this case the Erlang build tree will *not* be removed (as it is
		# more convenient for "more advanced" usage):
		#
		do_remove_build_tree=1

   fi

else

	prefix="${read_parameter}/Erlang/Erlang-${erlang_version}"

	if [ -d "${prefix}" ]; then
		echo "Using (pre-existing) '${prefix}' as installation directory."
	else
		echo "Using (non-existing yet) '${prefix}' as installation directory."
	fi

fi


if [ -n "${cpu_limit_expr}" ]; then

	echo "The slowing down of the build is enabled and the cpulimit tool has been found, so limiting the CPU usage of that build to ${cpu_limit_percentage}% of a single core."

elif [ $do_cpulimit -eq 0 ]; then

	echo "Warning: slow down enabled, yet no 'cpulimit' executable found, so no such limitation is to take place (except 'nice', if enabled)." 1>&2

fi


#echo "do_download = $do_download"
#echo "do_manage_doc = $do_manage_doc"
#echo "do_generate_plt = $do_generate_plt"

#echo "build_user=${build_user}"

# On official site or on Github:
erlang_src_prefix="otp_src_${erlang_version}"
#erlang_src_prefix="otp-OTP-${erlang_version}"

erlang_src_archive="${erlang_src_prefix}.tar.gz"

# As documentation archives were not generated for patch versions (e.g. 24.1.4),
# only for "base" versions" (e.g. 24.1):
#
#erlang_doc_prefix="$(echo "otp_doc_html_${erlang_version}" | awk -F. '{print otp_doc_html_$1"."$2""}')"


# on Github now:
#erlang_doc_archive="${erlang_doc_prefix}.tar.gz"
erlang_doc_archive="otp_doc_html_${erlang_version}.tar.gz"


# Some early checkings:

if [ ! -e "/usr/include/ncurses.h" ]; then

	echo "  Error, the libncurses headers cannot be found, whereas they are needed for the build.
Use for instance 'apt-get install libncurses5-dev' (other packages should preferably be also installed beforehand, refer to the help message displayed thanks to the ${help_opt_short} option)." 1>&2

	exit 5

fi


# Necessary for crypto, generally wanted:
check_ssl=0

if [ $check_ssl -eq 0 ]; then

	ssl_header="/usr/include/openssl/ssl.h"

	if [ ! -f "${ssl_header}" ]; then

		echo "Warning: no SSL header found (no '${ssl_header}'), probably that the crypto module will be lacking. Continue the build anyway? (y/n) [n]" 1>&2

		read res

		if [ ! "${res}" = "y" ]; then

			echo "  Build stopped. Consider installing the SSL headers (typically a 'libssl-dev' package)." 1>&2

			exit 15

		fi

	fi

fi


if [ $do_patch -eq 0 ]; then

	patch_tool="$(which patch)"

	if [ ! -x "${patch_tool}" ]; then

		echo "  Error, the patching of the Erlang sources was requested, but the 'patch' utility cannot be found on this system. Either install it (e.g. 'apt-get install patch') or use the ${patch_opt_long} option." 1>&2

		exit 6

	fi

fi


if [ -n "${nice_opt}" ]; then

	echo "Warning: this installation is intentionally slowed down with 'nice' to better share computing resources." 1>&2

fi

if [ -n "${cpu_limit_expr}" ]; then

	echo "Warning: this installation is intentionally slowed down with 'cpulimit' to avoid any overheat." 1>&2

fi


#md5sum="$(which md5sum 2>/dev/null)"

# Typically as /usr/bin/sha512sum ('coreutils' Arch package); 'shasum
# --algorithm 512' ('perl' Arch package) or 'openssl dgst -sha512' ('openssl'
# Arch package) could be used as well:
#
sha512sum="$(which sha512sum 2>/dev/null)"

if [ ! -x "${sha512sum}" ]; then

	echo "  Error: no checksum tool ('sha512sum') found available." 1>&2

	exit 17

fi

# Archives are not available by default:
src_available=1
doc_available=1

src_checked=1

if [ $do_download -eq 0 ]; then

	# However we check it is not already available in the current directory:
	if [ -f "${erlang_src_archive}" ]; then

		sum_res=$(${sha512sum} "${erlang_src_archive}")

		computed_sum=$(echo "${sum_res}" | awk '{printf $1}')

		if [ "${computed_sum}" = "${erlang_sum}" ]; then

			echo "SHA512 sum for the Erlang source archive already locally available validated, not downloading the archive, using that version."
			src_available=0
			src_checked=0

		fi

	fi

	if [ -f "${erlang_doc_archive}" ]; then

		# Checksum for doc not deemed useful enough:
		doc_available=0

	fi

	if [ $src_available -eq 1 ]; then

		erlang_target_src_url="${erlang_download_location}/${erlang_src_archive}"

		echo "Downloading now ${erlang_target_src_url}"
		set_wget
		if ! ${sudo_cmd} ${wget} ${wget_opts} "${erlang_target_src_url}" ; then
			echo "  Error while downloading ${erlang_target_src_url}, quitting." 1>&2
			exit 15
		fi

	fi

	erlang_target_doc_url="${erlang_download_location}/${erlang_doc_archive}"

	if [ $do_manage_doc -eq 0 ]; then

		if [ $doc_available -eq 1 ]; then

			echo "Downloading now ${erlang_target_doc_url}"
			set_wget
			if ! ${sudo_cmd} ${wget} ${wget_opts} ${erlang_target_doc_url}; then
				echo "  Error while downloading ${erlang_target_doc_url}, quitting." 1>&2
				exit 16
			fi

		fi

	fi

else

	if [ ! -f "${erlang_src_archive}" ]; then

		echo "  Error, Erlang source archive (${erlang_src_archive}) could not be found from current directory ($(pwd)), and no download was requested." 1>&2
		exit 20

	fi


	if [ $do_manage_doc -eq 0 ]; then

		if [ ! -f "${erlang_doc_archive}" ]; then

			echo "  Error, Erlang documentation archive (${erlang_doc_archive}) could not be found, and no download was requested." 1>&2
			exit 21

		fi
	fi

fi


if [ ! ${src_checked} -eq 0 ]; then

	echo "Checksum for ${erlang_src_archive}"

	sum_res=$(${sha512sum} "${erlang_src_archive}")

	computed_sum=$(echo "${sum_res}" | awk '{printf $1}')

	if [ "${computed_sum}" = "${erlang_sum}" ]; then

		echo "SHA512 sum for Erlang source archive matches."
		src_checked=0

	else

		echo "Error, SHA512 sums not matching for Erlang source archive '${erlang_src_archive}': expected '${erlang_sum}', computed '${computed_sum}'." 1>&2

		exit 25

	fi

fi

# Sometimes, we have versions for urgent bugfixing (e.g. R15B03-1 instead of
# R15B03). In this case the archive is named 'otp_src_R15B03-1.tar.gz' but it
# contains a root directory named only 'otp_src_R15B03'.  So if
# erlang_src_prefix="otp_src_R15B03-1" then
# erlang_extracted_prefix="otp_src_R15B03":
#
# (a similar case happened with the otp_src_17.0-rc1 release, which was put in
# an otp_src_17 archive root directory)
#
erlang_extracted_prefix=$( echo "${erlang_src_prefix}" | sed 's|-[0-9]*$||' | sed 's|\.[0-9]*-rc[0-9]*$||' )

if [ $use_prefix -eq 0 ]; then

	echo "Erlang version ${erlang_version} will be installed in ${prefix}."

	${sudo_cmd} ${mkdir} -p "${prefix}"

	# Removes any previous extracted directory, renamed or not:
	if [ -e "${erlang_extracted_prefix}" ]; then

		${sudo_cmd} ${rm} -rf "${erlang_extracted_prefix}"

	fi

	if [ -e "${erlang_src_prefix}" ]; then

		${sudo_cmd} ${rm} -rf "${erlang_src_prefix}"

	fi

else

	echo "Erlang version ${erlang_version} will be installed in the system tree."

	# Nevertheless some cleaning is to be performed, otherwise Dialyzer may
	# catch multiple versions of the same BEAM:
	#
	${rm} -rf /usr/local/lib/erlang

fi


if ! ${sudo_cmd} ${nice_opt} ${cpu_limit_expr} ${tar} xf "${erlang_src_archive}"; then
	echo "  Error while extracting ${erlang_src_archive}, quitting." 1>&2
	exit 50
fi

initial_path="$(pwd)"

# Corrects any extracted root directory, like 'R15B03' instead of 'R15B03-1':
if [ ! -d "${erlang_src_prefix}" ]; then

	if [ -d "${erlang_extracted_prefix}" ]; then

		${sudo_cmd} ${mv} -f "${erlang_extracted_prefix}" "${erlang_src_prefix}"

	else

		echo "  Error, no extracted directory (${erlang_extracted_prefix}) found." 1>&2
		exit 60

	fi

fi

# Starting from the source tree:

cd "${erlang_src_prefix}"


# Apparently not needed since Erlang 21.0, where 'infinity' is specified in
# terms of time-out:
#
if [ $do_patch -eq 0 ]; then

	echo "Patching first the Erlang sources."

	cd lib/kernel/src

	# Patch effects:
	#
	# - in lib/kernel/src/auth.erl, set a 30-second time-out for get_cookie
	# requests instead of the default 5 seconds, otherwise a time-out may
	# occur if using a few dozens of nodes:
	# "{timeout,{gen_server,call,[auth,{get_cookie,'SOME_NODE..."

	# First, generate the patch file with an "Here Document":
	(
		cat <<End-of-script
--- auth.erl    2011-06-23 13:32:45.557984017 +0200
+++ auth.erl-fixed      2011-06-23 13:32:45.557984017 +0200
@@ -106,7 +106,7 @@
 get_cookie(_Node) when node() =:= nonode@nohost ->
	 nocookie;
 get_cookie(Node) ->
-    gen_server:call(auth, {get_cookie, Node}).
+    gen_server:call(auth, {get_cookie, Node}, 30000).

 -spec set_cookie(Cookie :: cookie()) -> 'true'.

End-of-script

	) > ceylan-auth.patch

	if ! ${sudo_cmd} ${patch_tool} -p0 < ceylan-auth.patch; then

		echo "Error, the patching of Erlang sources (auth.erl) failed." 1>&2

		exit 55

	fi

	${sudo_cmd} ${rm} -f ceylan-auth.patch

	cd ../../..

fi


# See also:
# http://www.erlang-consulting.com/thesis/tcp_optimisation/tcp_optimisation.html
# for feature impact on performances.

# SSL by default is not supposed to be available. Hence for example the crypto
# module will not be available.
# Add below for example '--with-ssl=/usr/bin' to activate it.
# crypto could be still disabled due to:
# 'OpenSSL is configured for kerberos but no krb5.h found'.
#
# Now the JIT support is too important to allow not having it (a C++17 compiler
# is thus required):
#
configure_opt="--enable-threads --enable-smp-support --enable-kernel-poll --enable-hipe --enable-jit"

# Uncomment if building from a pre-Pentium4 computer:
#configure_opt="${configure_opt} --enable-ethread-pre-pentium4-compatibility enable_ethread_pre_pentium4_compatibilit=yes"

if [ $use_prefix -eq 0 ]; then
	prefix_opt="--prefix=${prefix}"
fi

echo "  Building Erlang environment..."

if ! ${sudo_cmd} ${nice_opt} ${cpu_limit_expr} ./configure ${configure_opt} ${prefix_opt}; then

	echo "Configuration failed, exiting." 1>&2
	exit 60

fi


if ! ${sudo_cmd} ${nice_opt} ${cpu_limit_expr} make; then

	echo "Build failed, exiting." 1>&2
	exit 61

fi


# No sudo here:
if ! ${nice_opt} ${cpu_limit_expr} make install; then

	echo "Installation failed, exiting." 1>&2
	exit 62

fi


echo "  Erlang successfully built and installed in ${prefix}."


# More global than 'if [ $use_prefix -eq 0 ]; then' so that most installs
# include these links:
#
if [ -n "${prefix}" ]; then

	# First, let's create a symbolic link so that this new version can be
	# transparently used by emacs:
	#
	cd "${prefix}/lib/erlang"

	# Exactly one match expected for the wildcard (e.g. tools-2.8.2), useful to
	# avoid having to update our ~/.emacs.d/init.el file whenever the 'tools'
	# version changes:
	#
	${ln} -sf lib/tools-*/emacs

	# Same story so that the crashdump viewer can be found irrespective of the
	# Erlang version:
	#
	${ln} -sf lib/observer-*/priv/bin/cdv

	# The same for JInterface:
	${ln} -sf lib/jinterface-* jinterface

	# Then go again in the install (not source) tree to create the base link:
	cd "${prefix}/.."

	# So we are in ${base_install_dir} now.

	# Sets as current:
	if [ -e "Erlang-current-install" ]; then

		${rm} -f Erlang-current-install

	fi

	${ln} -sf "Erlang-${erlang_version}" Erlang-current-install

fi


if [ $do_manage_doc -eq 0 ]; then

	if [ $use_prefix -eq 0 ]; then

		cd "${prefix}/.."

	else

		cd /usr/share

	fi

	# No sudo from there, as we have to use any right needed (for example to
	# write in the system tree)

	erlang_doc_root="Erlang-${erlang_version}-documentation"

	if [ -e "${erlang_doc_root}" ]; then

		${rm} -rf "${erlang_doc_root}"

	fi

	${mkdir} "${erlang_doc_root}"

	cd "${erlang_doc_root}"

	if ! ${nice_opt} ${cpu_limit_expr} ${tar} xf "${initial_path}/${erlang_doc_archive}"; then
		echo "  Error while extracting ${erlang_doc_archive}, quitting." 1>&2
		exit 70
	fi

	cd ..

	# Sets as current:
	if [ -e "Erlang-current-install" ]; then

		${rm} -f Erlang-current-documentation

	fi

	${ln} -sf "${erlang_doc_root}" Erlang-current-documentation

	echo "Erlang documentation successfully installed."

fi



if [ $do_remove_build_tree -eq 0 ]; then

	${rm} -rf "${initial_path}/${erlang_src_prefix}"

else

	echo "(the otp_src_${erlang_version} build directory can be safely removed if wanted)."

fi


echo

if [ -n "${prefix}" ]; then
	echo "The Erlang environment was successfully installed in ${prefix}."
else
	echo "The Erlang environment was successfully installed in its standard location."
fi


if [ $do_generate_plt -eq 0 ]; then

	if [ $use_prefix -eq 1 ]; then
		prefix="/usr/local"
	fi

	actual_plt_file="${prefix}/${plt_file}"
	actual_plt_link="${prefix}/${plt_link}"

	dialyzer_exec="${prefix}/bin/dialyzer"
	erlang_beam_root="${prefix}/lib/erlang"

	cd "${prefix}"

	if [ ! -x "${dialyzer_exec}" ]; then

		echo "  Error, no executable dialyzer found (tried '${dialyzer_exec}'), quitting." 1>&2
		exit 75

	fi


	if [ ! -d "${erlang_beam_root}" ]; then

		echo "  Error, root of Erlang BEAMs not found (tried '${erlang_beam_root}'), quitting." 1>&2
		exit 80

	fi


	echo

	#echo "Generating now a PLT file for that Erlang install (from ${erlang_beam_root}), in ${actual_plt_file} (using ${dialyzer_exec}). Note that this operation may be quite long (possibly one hour)."

	# Less detailed:
	echo "Generating now a PLT file for that Erlang install in ${actual_plt_file}. Note that this operation may be quite long (possibly one hour)."

	# In R17.1, dialyzer is not able to dereference symlinks, so instead of
	# generating with '--output_plt ${actual_plt_file}' and doing '${ln} -s
	# ${actual_plt_file} ${actual_plt_link}' we proceed the other way round:

	# sudo left out here, as PLT file might be in system tree:
	#
	# (this is the place where cpulimit is the most useful)
	#
	${nice_opt} ${cpu_limit_expr} ${dialyzer_exec} --build_plt -r ${erlang_beam_root} --output_plt ${actual_plt_link}
	res=$?

	if [ ${res} -eq 0 ]; then
		echo "The Erlang PLT file was successfully generated."
	elif [ ${res} -eq 2 ]; then
		echo "The Erlang PLT file was generated, but warnings were issued."
	else
		echo "  Error, the PLT generation failed (error code: ${res})." 1>&2
		exit 90
	fi

	# To include a PLT without knowing the current Erlang version:
	# (reversed symlink better than a copy)
	#
	${ln} -sf "${actual_plt_link}" "${actual_plt_file}"

fi

echo "
If wanting to generate a list of all the declared types in this Erlang distribution, and if having the 'Myriad' package, you can run: 'cd Ceylan-Myriad && make generate-list-of-erlang-types ERLANG_SOURCE_ROOT=${prefix}'."
