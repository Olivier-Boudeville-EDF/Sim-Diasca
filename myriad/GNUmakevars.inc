# Prerequisite: MYRIAD_TOP must be set.


# On conditional variables depending on conditionals.
#
# The make conditionals (such as ifeq, ifndef, etc.) are evaluated statically
# (immediately, and once for all) when the makefile is just read - not
# dynamically.
#
# As a result, defining (especially at the very end of the included makefiles)
# default settings for flag-like variables (such as ENABLE_DEBUG,
# EXECUTION_TARGET, etc.) like:
#
# ifndef ENABLE_DEBUG
#	ENABLE_DEBUG = true
# endif
#
# is of no use for any past conditional in the same file or in any upper layer
# (hence above in the expanded resulting makefile) that would rely on that
# variable.
#
# For example, any prior:
#
# ifeq ($(ENABLE_DEBUG),true)
#	ENABLE_FOOBAR_DEBUG_OPT = -Dfoobar_debug_mode
# endif
#
# will not set ENABLE_FOOBAR_DEBUG_OPT, as ENABLE_DEBUG itself will not be set
# yet: the conditional-based setting of a variable can only impact the
# conditionals *below* - not above.
#
# And using simply expanded variables (based on ':=') or recursively expanded
# ones (assigned with '=') does not matter here, as conditionals will be
# evaluated all the same.
#
# An overly complex and inconvenient solution would be to resolve these flags
# only when executing rules (hence through shell evaluation).
#
# This means that either each layer shall enforce its defaults first, or that
# all GNUmakevars files shall be split in a flag file (for conditionals) and a
# set files (for variables), included like:
#   a.GNUmakevars-flags
#   b.GNUmakevars-flags
#   a.GNUmakevars-set
#   b.GNUmakevars-set
#
# This is also an inconvenient solution, and a given variable being set may
# become in turn a flag anyway, so such a split would be somewhat arbitrary.
#

# As a result: any conditional-based variable that may possibly impact other
# conditional shall be set at the top of the GNUmakevars.inc of a given layer,
# and such a setting shall be done that way by *each* layer making use of that
# variable (no factoring in lower layers possible).
#
# This is notably the case for EXECUTION_TARGET, ENABLE_DEBUG, etc.


# So:


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


# We have to define a symbol specific to the 'production' execution target
# (exec_target_is_production) instead of relying only on an execution_target
# variable that could be set either to 'development' or 'production', as the
# Erlang preprocessor allows only to test whether a symbol is defined (with
# -ifdef), we cannot test its value.
#
# So the code is to use '-ifdef(target_production_exec)' instead of something
# like '-ifdef(execution_target,production)'.
#
ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_DEBUG = true
	ENABLE_NATIVE_COMPILATION = false

	# No exec_target_is_production shall be defined here:
	ERLANG_COMPILER_EXEC_TARGET_OPT =

	ERLANG_COMPILER_DEBUG_OPT = -Dmyriad_debug_mode

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_DEBUG = false

	# Native compilation may be enabled - with care; yet problems with
	# orddict:erase/2, lists:fold/3, etc. made us switch it off for the moment:
	#
	#ENABLE_NATIVE_COMPILATION = true
	ENABLE_NATIVE_COMPILATION = false

	ERLANG_COMPILER_EXEC_TARGET_OPT = -Dexec_target_is_production

	ERLANG_COMPILER_DEBUG_OPT =

else

	$(error Invalid Myriad execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif



# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#  - code_path: how the VM code path is managed
#  - otp_integration: how OTP conventions (notably regarding applications) are
#	 handled
#  - web_exchanges: http/https exchanges
#  - strings: notably for mismatches between plain and binary ones
#
# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	#MYRIAD_DEBUG_FLAGS += -Dmyriad_debug_code_path
	#MYRIAD_DEBUG_FLAGS += -Dmyriad_debug_otp_integration
	#MYRIAD_DEBUG_FLAGS += -Dmyriad_debug_web_exchanges
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	# MYRIAD_CHECK_FLAGS += -Dmyriad_check_strings
endif


# As, with Myriad, the trace system relies on console output, the default here
# is to ellipse emitted traces of lesser severity; uncomment next line to
# disable any ellipsing of them (in order to always have all traces in full):
#
# (note that, for such a make variable to be assigned by an upper layer and to
# be taken into account here, Myriad must be built from an overall, integrated
# Make context - not as a series of separate builds)
#
#MYRIAD_UNELLIPSED_TRACES_OPT = -Dmyriad_unellipsed_traces



# Allows to add stacktraces to the most commonly exceptions raised (ex:
# {not_a_string, Term}), independently of debug settings:
#
# (useful in contexts where stacktraces tend to vanish)
#
MYRIAD_DEBUG_FLAGS += -Dmyriad_add_stacktraces


ERLANG_COMPILER_OPT_BASE += $(MYRIAD_DEBUG_FLAGS)             \
							$(MYRIAD_CHECK_FLAGS)             \
							$(MYRIAD_UNELLIPSED_TRACES_OPT)




# Useful to target for example the root of the current layer (ex: to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(MYRIAD_TOP)
endif


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := Ceylan-Myriad
endif


# PROJECT_NAME_FOR_REBAR should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_REBAR
	PROJECT_NAME_FOR_REBAR := myriad
endif


# The uniquely-defined version of this layer, for all uses (including rebar and
# hex):
#
MYRIAD_VERSION := 1.0.17


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(MYRIAD_VERSION)
endif


# Variable name intentionally not including the current layer, for more generic
# rules:

ifndef VERSION_FOR_REBAR
	VERSION_FOR_REBAR := $(PROJECT_VERSION)
endif


ifndef PACKAGE_NAME
	PACKAGE_NAME := $(PROJECT_NAME)
endif


ifndef PACKAGE_TOP
	PACKAGE_TOP := $(MYRIAD_TOP)
endif


# Needed, exactly as is (make trick to specify space as a parameter):
space :=
space +=


ARCHIVE_ROOT := $$HOME/Archives/$(PROJECT_NAME)


# For full archives (including untracked files and VCS state):
ARCHIVE_FILE := $(ARCHIVE_ROOT)/$$(date +'%Y%m%d')-$(PROJECT_NAME).tar.xz

# For archives as considered by the VCS system (ex: non-staged files are not
# archived here):
#
VCS_ARCHIVE_FILE := $(ARCHIVE_ROOT)/$$(date +'%Y%m%d')-$(PROJECT_NAME).git-bundle

# For very light archives, just the bleeding edge of the current sources, with
# no VCS state:
#
LIGHT_ARCHIVE_FILE := $(ARCHIVE_ROOT)/$$(date +'%Y%m%d')-$(PROJECT_NAME)-light-archive.tar.xz


ARCHIVE_BRANCH_FILE := $(ARCHIVE_ROOT)/$$(date +'%Y%m%d')-$(PROJECT_NAME)-branch-$(shell basename $$(pwd)).tar.xz


SNAPSHOT_ARCHIVE_FILE := $(ARCHIVE_ROOT)/$$(date +'%Y%m%d')-$(PROJECT_NAME)-snapshot.tar.xz


PROJECT_RELEASE_BASE := $(PROJECT_NAME)-$(PROJECT_VERSION)

PROJECT_RELEASE_ARCHIVE_ZIP := $(PROJECT_RELEASE_BASE).zip
PROJECT_RELEASE_ARCHIVE_BZ2 := $(PROJECT_RELEASE_BASE).tar.bz2
PROJECT_RELEASE_ARCHIVE_XZ  := $(PROJECT_RELEASE_BASE).tar.xz


# Useful to define a Make variable without setting a value to it (ex: in order
# to be able to specifically define at a given layer a variable MY_VAR, to
# prevent that, at the layer below, a 'ifndef MY_VAR' is triggered; see
# BOOTSTRAP_MODULES in Ceylan-Traces for an example of it):
#
empty =



# Installation section (custom one, now mostly deprecated in favor of the OTP
# one, done through rebar3).


# If wanting to install that package, following installation prefix is used.
#
# INSTALLATION_PREFIX should be defined on a per-project basis, or be overridden
# on the command line (ex: 'make install INSTALLATION_PREFIX=/tmp'):

ifndef INSTALLATION_PREFIX
	INSTALLATION_PREFIX := $$HOME/Software
endif


PACKAGE_INSTALLATION_PREFIX := $(INSTALLATION_PREFIX)/$(PACKAGE_NAME)


# Could rely on the permissions instead:
#
# (not taking symbolic links to avoid duplicate targets)
#
EXEC_TO_INSTALL := $$(find $(PACKAGE_TOP) -name '*.sh' -a ! -name '*.exe' -a ! -type l)



BEAMS_TO_INSTALL := $$(find $(PACKAGE_TOP) -name examples -prune -o \( -name '*.beam' -a ! -name '*_test.beam' -a ! -name 'class_Test*.beam' \) -a -print)

SOURCES_TO_INSTALL := $$(find $(PACKAGE_TOP) -name examples -prune -o \( -name '*.erl' -a ! -name '*_test.erl' -a ! -name 'class_Test*.erl' \) -a -print)


EXAMPLES_TO_INSTALL_BEAMS := $$([ ! -d examples ] || find $(PACKAGE_TOP)/examples -name '*.beam')

EXAMPLES_TO_INSTALL_SRC := $$([ ! -d examples ] || find $(PACKAGE_TOP)/examples -name '*.erl')


INCLUDES_TO_INSTALL := $$(find $(PACKAGE_TOP) -name '*.hrl')


TESTS_TO_INSTALL_BEAMS := $$(find $(PACKAGE_TOP) -name '*_test.beam' -o -name 'class_Test*.beam')

TESTS_TO_INSTALL_SRC := $$(find $(PACKAGE_TOP) -name '*_test.erl' -o -name 'class_Test*.erl')


DOC_TO_INSTALL := $$(find $(PACKAGE_TOP) -name tmp-rst -prune -o \( -type f -a \( -name '*.pdf' -o -name '*.html' \) \) | grep -v tmp-rst)



# Install locations:

BIN_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/bin
EBIN_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/ebin
SRC_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/src
INCLUDE_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/include
EXAMPLES_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/examples
TEST_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/test
DOC_INSTALLATION_PREFIX := $(PACKAGE_INSTALLATION_PREFIX)/doc

DEFAULT_PRODUCTION_INSTALLATION_PREFIX := /usr/local/$(PACKAGE_NAME)


# Source section.
MYRIAD_SRC := $(MYRIAD_TOP)/src

# BEAM path section.
MYRIAD_BEAM := $(MYRIAD_TOP)/src

# Contributions path section:
#MYRIAD_CONTRIB := $(MYRIAD_TOP)/contrib


# BEAM_DIRS are relative paths of BEAM files, ABS_BEAM_DIRS are the
# corresponding absolute paths.
#
# As the 'include' structure matches the one of 'src' (only drawback: includes a
# non-necessary 'ebin' directory):
#
INC += $(foreach dir,$(BEAM_DIRS),-I$$(echo $(dir)|sed 's|src|include|1'))


# Special-cased includes for bootstrapped modules, which by default are
# associated to a wrong directory prefix, as their MYRIAD_TOP variable points at
# the Myriad root instead of their actual source directory:
#
BOOTSTRAP_INCLUDES := -I$(MYRIAD_TOP)/../../include/meta      \
					  -I$(MYRIAD_TOP)/../../include/utils


# Extra prerequisite includes:

# Optional services, relying on third-party prerequisites:
#
# (for them, we do not force the corresponding variable here, in order to let
# the upper layers be able to decide; if not, the defaults apply)


# For the reading and writing of large, often scientific-oriented data:
#USE_HDF5 = false
#USE_HDF5 = true

# To gain the ability of acting as a client of a REST service:
#USE_REST = false
#USE_REST = true

# To be able to handle JSON content:
#USE_JSON = false
#USE_JSON = true

# For SQL database operations, based on SQLite 3 operations:
#USE_SQLITE = false
#USE_SQLITE = true

# For using an Erlang/Python binding, based on the ErlPort library:
#USE_PYTHON_BINDING = false
#USE_PYTHON_BINDING = true

# For using an Erlang/Java binding, based on the JInterface library:
#USE_JAVA_BINDING = false
#USE_JAVA_BINDING = true



# Deactivated by default, as requires specific prerequisites (the HDF5 library
# and the erlhdf5 binding) at runtime and also at compilation time (includes
# needed).
#
ifndef USE_HDF5
	USE_HDF5 = false
endif


ifeq ($(USE_HDF5),true)

	# If using erlhdf5 (for the hdf5_support module):

	ERLHDF5_BASE := $$HOME/Software/erlhdf5

	INC += -I$(ERLHDF5_BASE)/include

	# (ebin/erlhdf5.beam and priv/erlhdf5.so found at runtime)

endif



# Deactivated by default, as requires specific prerequisites (the selected
# third-party JSON parser backend) at runtime (not at compilation time, as not
# needing any specific include).
#
ifndef USE_REST
	USE_REST = false
endif


ifeq ($(USE_REST),true)
	USE_JSON = true
endif

ifeq ($(USE_JSON),true)

	 # At least one parser backend shall then be selected:

	# Default choice:
	USE_JSX := true
	#USE_JSX := false

	#USE_JIFFY := true
	USE_JIFFY := false

	ifeq ($(USE_JSX),true)

		# The JSX JSON parser is available at
		# https://github.com/talentdeficit/jsx.

		# Install obtained thanks to:
		#
		# $ mkdir -p ~/Software/jsx
		# $ cd ~/Software/jsx
		# $ git clone git@github.com:talentdeficit/jsx.git
		# $ ln -s jsx jsx-current-install
		# $ cd jsx/
		# $ rebar3 compile && rebar3 eunit

		# Actually not used, as decided at runtime rather than at compile time:
		#JSON_OPT = -Duse_jsx_json_backend

		JSX_BASE := $$HOME/Software/jsx/jsx-current-install

		JSX_INSTALL_BASE := $(JSX_BASE)/_build/default/lib/jsx

		# No includes for JSX:
		#INC += -I$(JSX_INSTALL_BASE)/include

		# ($(JSX_INSTALL_BASE)/ebin/*.beam found at runtime)

		ABS_BEAM_DIRS += $(JSX_INSTALL_BASE)/ebin

	endif

	ifeq ($(USE_JIFFY),true)

		# The Jiffy JSON parser is available at
		# https://github.com/davisp/jiffy.

		# Install obtained thanks to:
		#
		# $ mkdir -p ~/Software/jiffy
		# $ cd ~/Software/jiffy
		# $ git clone git@github.com:davisp/jiffy.git
		# $ ln -s jiffy jiffy-current-install
		# $ cd jiffy/
		# $ rebar3 compile && rebar3 eunit

		# Actually not used, as decided at runtime rather than at compile
		# time:
		#
		#JSON_OPT = -Duse_jiffy_json_backend

		JIFFY_BASE := $$HOME/Software/jiffy/jiffy-current-install

		JIFFY_INSTALL_BASE := $(JIFFY_BASE)/_build/default/lib/jiffy

		# No includes for Jiffy:
		#INC += -I$(JIFFY_BASE)/include

		# ($(JIFFY_BASE)/ebin/*.beam found at runtime)

		ABS_BEAM_DIRS += $(JIFFY_INSTALL_BASE)/ebin

	endif

	ifeq ($(USE_JSX),false)
		 ifeq ($(USE_JIFFY),false)
			$(error JSON use enabled, yet no corresponding backend selected.)
		endif
	endif

endif



# Deactivated by default, as requires specific prerequisites:
#
# - the SQLite 3 library, ex: sqlite3 and sqlite3-dev packages in Debian, sqlite
#    on Arch Linux
# - the SQLite 3 Erlang binding we retained, erlang-sqlite3 (cf
#    https://github.com/alexeyr/erlang-sqlite3.git)
#
ifndef USE_SQLITE
	USE_SQLITE = false
endif


ifeq ($(USE_SQLITE),true)

	SQLITE3_BASE := $$HOME/Software/erlang-sqlite3

	INC += -I$(SQLITE3_BASE)/include

	# ($(SQLITE3_BASE)/ebin/*.beam found at runtime)

	ABS_BEAM_DIRS += $(SQLITE3_BASE)/ebin/

endif



# Deactivated by default, as requires specific prerequisites:
# - Python (binding tested with v3.6+ only)
# - the ErlPort library (cf https://github.com/hdima/erlport)
#
ifndef USE_PYTHON_BINDING
	USE_PYTHON_BINDING = false
endif

ifeq ($(USE_PYTHON_BINDING),true)

  ERLPORT_BASE := $$HOME/Software/ErlPort/ErlPort-current-install

  INC += -I$(ERLPORT_BASE)/src

  # ($(ERLPORT_BASE)/ebin/*.beam found at runtime)

  ABS_BEAM_DIRS += $(ERLPORT_BASE)/ebin

endif



# Deactivated by default, as requires specific prerequisites:
# - Java (binding tested with 8.0+ only)
# - the JInterface library, bundled with the Erlang runtime (cf
#   http://erlang.org/doc/man/jinterface.html)
#
ifndef USE_JAVA_BINDING
	USE_JAVA_BINDING = false
endif

ifeq ($(USE_JAVA_BINDING),true)

	# The path pointing to the target JInterface JAR (OtpErlang.jar), only
	# needed at compilation-time:
	#
	# Ex: cd [...]/Erlang-current-install/lib/erlang &&
	#     ln -s lib/jinterface-1.8.1 jinterface-current-install
	#
	JINTERFACE_ROOT = $(ERLANG_ROOT)/lib/erlang/jinterface

	CLASSPATH_DIRS += $(JINTERFACE_ROOT)/priv/OtpErlang.jar .

	# We need to have all specified paths separated with ':':
	CLASSPATH_OPT = -classpath $(subst $(space),:,$(strip $(CLASSPATH_DIRS)))

	JAVAC_OPT = $(CLASSPATH_OPT)

	# Only useful for manual testing:
	JAVA_OPT = $(CLASSPATH_OPT):.

endif



# To know when the including makefile is the sole root one:
ifndef ROOT_MAKEFILE
	ROOT_MAKEFILE = false
endif

# Unless specified otherwise, the including makefile is not the one used as the
# base of a given layer (hence, for example, it is not expected to build in
# parallel):
#
ifndef BASE_MAKEFILE
  BASE_MAKEFILE = false
endif


# Overriding BASE_PLT is useful to take into account third-party prerequisites:
ifndef BASE_PLT
  BASE_PLT = $(PREDECESSOR_PLT)
endif



# To test conditional checks:
ERLANG_COMPILER_TEST_TOKEN_OPT := -Dmy_first_test_token            \
								  -Dmy_second_test_token=200       \
								  -Dmy_third_test_token=some_text


# Allows to list the tokens used for the enabling of conditional code:
ERLANG_COMPILER_TOKEN_OPT := $(ERLANG_COMPILER_TEST_TOKEN_OPT)


ALL_ERL_FILES = $(wildcard *.erl)


# Excluded sources, as depending on prerequisites:


ifeq ($(USE_HDF5),false)

	EXCLUDED_SOURCES := $(EXCLUDED_SOURCES) hdf5_support.erl      \
											hdf5_support_test.erl \
											raw_hdf5_test.erl

endif


ifeq ($(USE_JSON),false)

	EXCLUDED_SOURCES := $(EXCLUDED_SOURCES) \
						json_support.erl json_support_test.erl \
						rest_support.erl rest_support_test.erl

endif


ifeq ($(USE_SQLITE),false)
	EXCLUDED_SOURCES := $(EXCLUDED_SOURCES) sql_support.erl sql_support_test.erl
endif


ERL_FILES = $(filter-out $(EXCLUDED_SOURCES), $(ALL_ERL_FILES))


# Includes test beams:
BEAM_FILES = $(patsubst %.erl,%.beam,$(ERL_FILES))


# Excludes the tests depending on prerequisites:
TEST_SOURCES = $(filter-out $(EXCLUDED_SOURCES), $(wildcard *_test.erl))

TEST_TARGETS = $(patsubst %.erl,%,$(TEST_SOURCES))



# Java subsection.

# Java (local) source files:
JAVA_FILES = $(wildcard *.java)


# Java class (bytecode) files:
ifeq ($(USE_JAVA_BINDING),true)
	JAVA_CLASS_FILES = $(patsubst %.java,%.class,$(JAVA_FILES))
endif



# Regarding dependency management.

# One can just ignore dependencies and rebuild "blindly" every module in current
# subtree.

# One just has not to forget to issue 'make' from a right location, before
# running one's Erlang program.

# Another option is to track dependencies explicitly.

# We found no existing way of managing dependencies automatically, thus they
# have to be declared by hand.

# We want these user-specified dependencies to work with the generic rules to
# build beam files and to run programs.

# For example, 'make MyObject_run' would check MyObject_test.beam and its
# dependencies before running it.

# A generic rule like: '%.beam: %.erl' would not take into account dependencies.

# A generic rule like: '%.beam: %.erl %_dependencies' with a definition like
# 'MyObject_dependencies: A.beam moduleB.beam' would work (and modules not
# depending on others could be managed with a second generic rule: '%.beam:
# %.erl'), but generic targets, as defined in GNUmakerules-automatic.inc, seem
# to be *always* rebuilt, not depending on their .PHONY status.

# In our case, MyObject.beam (and all its prerequisites!) would thus always be
# rebuilt, even if no change at all was operated on the corresponding sources,
# which would not be acceptable.

# Thus we stick from now to the basic strategy: always issue a global 'make' at
# the root of the source before running a program, otherwise be doomed (changes
# not taken into account at run-time, as not recompiled).




# Enforce default values:

# Expected to be already set through EXECUTION_TARGET:
ifndef ENABLE_DEBUG
	ENABLE_DEBUG = true
endif


ifndef ENABLE_NATIVE_COMPILATION
	ENABLE_NATIVE_COMPILATION = false
endif


# Apply settings:

# Tells which set of settings should be applied (allows for conditional code):
# (not relevant anymore, see above)
#
#ERLANG_COMPILER_EXECUTION_TARGET_OPT += -Dexecution_target=$(EXECUTION_TARGET)

ifeq ($(ENABLE_NATIVE_COMPILATION),true)
	ERLANG_COMPILER_NATIVE_COMPILATION_OPT = +native
else
	ERLANG_COMPILER_NATIVE_COMPILATION_OPT =
endif




# Erlang environment section.

ERLANG_USUAL_ROOT := ~/Software/Erlang/Erlang-current-install


ERLANG_COMPILER := $$(PATH=$(ERLANG_USUAL_ROOT)/bin:$$PATH which erlc 2>/dev/null)
# To check actual compile options:
#ERLANG_COMPILER := echo "COMPILE OPTIONS:"


ERLANG_BIN_ROOT := $(shell dirname $(ERLANG_COMPILER) )

ERLANG_ROOT := $(shell dirname $(ERLANG_BIN_ROOT) )


# Note that at least some versions of dialyzer (ex: the one shipped with 17.1)
# will not deference anymore symlinks (if Erlang.plt pointing to Erlang-17.1.plt
# for example, it will not be found; this is managed in our installation
# script):
#
ERLANG_PLT_FILE := $(ERLANG_ROOT)/Erlang.plt


WARNING_LEVEL := 2

# Used in all cases (even if not in debug mode), as useful:
ERLANG_COMPILER_OPT_DEBUG := +debug_info



# The number of available cores on this computer:
#
# (set it to 1 in order to restore a sequential build)
#
CORE_COUNT := $$(grep -c processor /proc/cpuinfo)
#CORE_COUNT := 1

ERLANG_INTERPRETER := $$(PATH=$(ERLANG_USUAL_ROOT)/bin:$$PATH which erl 2>/dev/null)


# Not using '-smp auto' anymore, as the SMP mode is needed even with a single
# core if GUI (WxWindows) is to be used:
#
# (currently not used)
#
#ERLANG_INTERPRETER_OPT_DEBUG = -init_debug -smp


# Force your FQDN, if ever relevant in your setting:
# FQDN := a_host_name.a_domain_name

ifdef FQDN

	FQDN_OPT = "--fqdn $(FQDN)"

else

	#FQDN := $$(host $(hostname) | awk '{ print $$1 }' | head -n 1)
	FQDN := $$(hostname -f)

  # Not useful in this case:
  #FQDN_OPT = "--fqdn $(FQDN)"

endif



ifndef VM_NAME
	VM_NAME := ceylan_myriad_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := ceylan_myriad_test
endif


ifndef COOKIE
	COOKIE := "ceylan_myriad_cookie"
endif


COOKIE_OPT = -c $(COOKIE)


# One may switch to short names if network configuration does not allow for long
# ones:
#
#DISTRIBUTED_NODE_OPT := -sname $(VM_NAME) -setcookie $(COOKIE)
DISTRIBUTED_NODE_OPT := -name $(VM_NAME)@$(FQDN) -setcookie $(COOKIE)


STARTUP_FUNCTION := run


# So that they can be fetched from outside the make system:
# (see the 'list-beam-dirs' target and code_utils:get_beam_dirs_for/1)
#
# (listed in the alphabetical order)
#
# Not far from being candidate for deprecation (next: schedule for removal), as
# top-level 'ebin' does the trick.
#
MYRIAD_BEAM_DIRS := $(MYRIAD_BEAM)/apps/generate-password       \
					$(MYRIAD_BEAM)/apps/merge-tool              \
					$(MYRIAD_BEAM)/data-management              \
					$(MYRIAD_BEAM)/maths                        \
					$(MYRIAD_BEAM)/meta                         \
					$(MYRIAD_BEAM)/scripts                      \
					$(MYRIAD_BEAM)/user-interface/src/graphical \
					$(MYRIAD_BEAM)/user-interface/src/textual   \
					$(MYRIAD_BEAM)/user-interface/src           \
					$(MYRIAD_BEAM)/utils



# Myriad's parent directory added to the code path, as any external code using
# Myriad is expected to reference its (public) headers thanks to, for example,
# '-include_lib("myriad/include/ast_info.hrl").'; knowing that -include_lib
# relies on the code path, not the header file path, we thus add Myriad's
# include directory to the code path.
#
# We also add the ebin directory, typically so that tests run with our make
# system yet from a rebar3 context can nevertheless find the Myriad BEAMs they
# rely on.
#
BEAM_DIRS += $(MYRIAD_BEAM_DIRS) $(MYRIAD_TOP)/.. $(MYRIAD_TOP)/ebin


ERL_LAUNCHER = $(MYRIAD_SRC)/scripts/launch-erl.sh



# Compiler section.


# Modules needed for the bootstrapping for others (hence to be built first, and
# not parse-transformed; typically *used* by parse-transforms, or being a
# parse-transform themselves)
#
# (see the 'Bootstrap section' in GNUmakerules-explicit.inc for their
# special-cased build)
#
ifndef BOOTSTRAP_MODULES

	# We want to bootstrap all relevant modules from 'meta':

	META_SRC_FILES = $(wildcard $(MYRIAD_TOP)/src/meta/*.erl)

	# As they do not need to be specifically bootstrapped:
	EXCLUDED_FROM_BOOTSTRAPPED_META =                                    \
				$(MYRIAD_TOP)/src/meta/myriad_parse_transform.erl        \
				$(MYRIAD_TOP)/src/meta/myriad_parse_transform_test.erl   \
				$(MYRIAD_TOP)/src/meta/example_parse_transform.erl       \
				$(MYRIAD_TOP)/src/meta/minimal_parse_transform_test.erl  \
				$(MYRIAD_TOP)/src/meta/simple_parse_transform_target.erl \
				$(MYRIAD_TOP)/src/meta/ast_generation_test.erl           \
				$(MYRIAD_TOP)/src/meta/meta_utils_test.erl               \
				$(MYRIAD_TOP)/src/meta/cond_utils_test.erl               \



	META_FILTERED_SRC_FILES = $(filter-out $(EXCLUDED_FROM_BOOTSTRAPPED_META),$(META_SRC_FILES))

	META_BEAM_FILES = $(patsubst %.erl,%.beam,$(META_FILTERED_SRC_FILES))


	# We list here all prerequisites of the 'Myriad' parse-transform:
	#
	# - basic_utils: to provide the basic services
	# - text_utils: to display debugging information regarding the transforms
	# - id_utils: used here to sort the forms
	# - most of 'meta', including:
	#    * meta_utils: to support the metaprogramming needs
	#    * ast_info: to centralise information regarding ASTs
	#    * ast_utils: to handle the AST
	#    * ast_scan: to scan the AST
	#    * type_utils: to handle types
	# - [not bootstrapped anymore] list_table: useful for tables that shall
	#	preserve the order of entries
	# - map_hashtable: to manage the module information
	#
	BOOTSTRAP_MODULES = $(MYRIAD_TOP)/src/utils/basic_utils.beam             \
						$(MYRIAD_TOP)/src/utils/text_utils.beam              \
						$(MYRIAD_TOP)/src/utils/id_utils.beam                \
						$(META_BEAM_FILES)                                   \
						$(MYRIAD_TOP)/src/data-management/map_hashtable.beam \

	# Delisted: $(MYRIAD_TOP)/src/data-management/list_table.beam    \

endif


## Parse-transform related section.


# List here all the directories expected to contain parse transforms:
#
# Note: such variables were meant to be overridden by each upper layer, yet
# generally these layers require the *addition* (not replacement) of new rules
# (ex: to manage classes) to the base ones (to be still able to compile standard
# modules).
#
ifndef PARSE_TRANSFORM_DIRS
	PARSE_TRANSFORM_DIRS = $(MYRIAD_TOP)/src/meta
endif


DEBUG_INFO_KEY := "Ceylan-Myriad"

DEBUG_INFO_KEY_OPT = +'{debug_info_key,$(DEBUG_INFO_KEY)}'


# General (base, transverse) compilation options that should be used for all
# modules (third-party included):
#
# -v/+verbose caused inlining to be too verbose, ex:
# "Old inliner: threshold=0 functions=[{setAttribute,3},.."
#
ERLANG_COMPILER_GENERAL_OPT += -b beam $(DEBUG_INFO_KEY_OPT)


# Variable defined to be re-usable by upper layers, so that compiling their own
# parse transforms can take advantage of the Myriad one:
#
# (the 'ebin' directory is listed here as, for projects built with rebar3 and
# depending on Myriad, the Myriad BEAMs exist only in the
# _build/default/lib/myriad/ebin directory, not anymore within the
# _build/default/lib/myriad/src tree)
#
ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MYRIAD =                \
					-pz $(MYRIAD_TOP)/src/utils                 \
					-pz $(MYRIAD_TOP)/src/meta                  \
					-pz $(MYRIAD_TOP)/src/data-management       \
					-pz $(MYRIAD_TOP)/ebin                      \
					'+{parse_transform,myriad_parse_transform}'


# To apply a parse transform, not only it must be already compiled, but also,
# when compiling the module to be transformed, the BEAM of the transform must be
# found:
#
# (add -P to generate, when compiling X.erl, a X.P file listing the transformed
#  sources that are actually compiled)
#
# We conditionally defined this option, as upper layers may want to trigger
# their own parse transforms *instead* (probably that will call directly, by
# themselves, at the Erlang level, this 'Myriad' parse transform):
#
#ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT

	   # We need src/{utils,meta,data-management} in the path, as the 'Myriad'
	   # parse transform uses meta modules that use map_hashtable (which is thus
	   # bootstrapped as well).

	   # Note: if, when compiling a module, the following error message is
	   # issued: "undefined parse transform 'myriad_parse_transform'", then it
	   # is most probably that the code of the parse transform actually calls,
	   # directly or not, a non-existing code (typically a function of a module
	   # to be built later)

	   #ERLANG_COMPILER_PARSE_TRANSFORM_OPT = \
	   #	$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MYRIAD)

#endif


ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES

	ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = \
		$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_MYRIAD)

endif


# Warning-related options (may or may not be included):
# "+{warn_format,2}" would trigger unwanted useless warnings about format
# strings. So the default "+{warn_format,1}" verbosity applies.

# Refer to https://erlang.org/doc/man/compile.html#file-2 for the complete list
# of warnings.

# So that this can be overridden:
#
# warn_unused_vars not listed here, yet disabled in production mode due to
# conditional sections being removed and resulting in variables being then
# reported as unused.
#
ifndef ERLANG_COMPILER_BASE_WARNING_OPT

	ERLANG_COMPILER_BASE_WARNING_OPT = -W$(WARNING_LEVEL) +warn_export_all \
		   +warn_export_vars +warn_shadow_vars +warn_unused_function       \
		   +warn_deprecated_function +warn_deprecated_type +warn_removed   \
		   +warn_obsolete_guards +warn_unused_import +warn_unused_record   \
		   +warn_nif_inline +warnings_as_errors

endif


 # See comment above about conditionals:
ifeq ($(EXECUTION_TARGET),production)
	 ERLANG_COMPILER_BASE_WARNING_OPT += +nowarn_unused_vars
else
	ERLANG_COMPILER_BASE_WARNING_OPT += +warn_unused_vars
endif


ERLANG_COMPILER_WARNING_OPT += $(ERLANG_COMPILER_BASE_WARNING_OPT)


# Once type specs will have been added: +warn_untyped_record +warn_missing_spec


# No general inlining is enforced, as:
#
# - the induced gain is not obvious, compared to the non-inlined version
#
# - inlining seems to be triggered only through a in-file directive, whereas no
# general include is used here
#
# Otherwise we would have added, on a per-file basis:
# -compile(inline).
# -compile( {inline_size,64} ).


# +bin_opt_info removed, as even efficient binary handling yields "warning"
# messages ("OPTIMIZED: creation of sub binary delayed"), which are treated as
# errors afterwards.
#
# Instead 'export ERL_COMPILER_OPTIONS=bin_opt_info' shall be punctually used.
#
ERLANG_COMPILER_OPT_BASE +=                \
 $(ERLANG_COMPILER_GENERAL_OPT) $(INC)     \
 $(ERLANG_COMPILER_EXEC_TARGET_OPT)        \
 $(ERLANG_COMPILER_DEBUG_OPT)              \
 $(ERLANG_COMPILER_NATIVE_COMPILATION_OPT) \
 $(ERLANG_COMPILER_WARNING_OPT)            \
 $(ERLANG_COMPILER_OPT_DEBUG)


# Base options to compile parse transforms themselves (and their helper
# modules):
#
ERLANG_COMPILER_OPT_FOR_PT += $(ERLANG_COMPILER_OPT_BASE)

ERLANG_COMPILER_OPT_FOR_BOOTSTRAPPED = $(ERLANG_COMPILER_OPT_FOR_PT) \
									   $(BOOTSTRAP_INCLUDES)



# Conditionally defined so that upper layers may update these settings (ex:
# adding pz directories):
#
ifndef ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES

	ERLANG_COMPILER_OPT_FOR_STANDARD_MODULES =                          \
		$(ERLANG_COMPILER_OPT_BASE)                                     \
		$(ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES)

endif


# Conditionally defined so that upper layers may update these settings:
ifndef ERLANG_COMPILER_OPT_FOR_TESTS

	ERLANG_COMPILER_OPT_FOR_TESTS = $(ERLANG_COMPILER_TOKEN_OPT)

endif


# For the Java binding; expecting Java 8 or higher:
JAVAC := $$(which javac)

# Optional variable, used for testing only (never directly executed from the
# shell):
#
JAVA := $$(which java)



# Runtime settings.

# Note that some runtime-related (interpreter/VM) defines are unused as we rely
# on the src/scripts/launch-erl.sh script instead (refer
# to the ERL_PARAMETERIZED_LAUNCHER variable).


# +v (verbose) option removed, as only active if the interpreter has been
# compiled in debug mode (otherwise a warning is displayed at each execution).
#
# (note: variable generally not used, as using the ERL_LAUNCHER script instead)
#
#ERLANG_INTERPRETER_OPT = -pz $(BEAM_DIRS) $(DISTRIBUTED_NODE_OPT)




# Erlang Default (only used in a node that is distributed):
#EPMD_PORT := 4369

# 'Myriad' default, to force correct port management:
#
# Note: if modifying this port definition, upper layers may have to modify that
# accordingly.
#
# Notably, if a VM is ultimately started with the --nn option (i.e. not
# distributed), enabling distribution at runtime (see
# net_utils:enable_distribution/2) will lead this VM to expect an EPMD at the
# port specified here (thus this information is taken into account in all cases,
# even when starting non-distributed)
#
# Always test with a prior 'killall epmd' on all hosts involved, and use 'epmd
# -d -port XXXX' to check whether there is an EPMD at that port (this is the
# case iff 'there is already a epmd running at port XXXX' is output)
#
EPMD_PORT := 4506
#EPMD_PORT := 4514


# We can force the launched VM to use TCP/IP connections in a range of ports
# enabled by the firewalls and/or to rely on a non-standard EPMD port:
#
# (note that, should the user code launch other VMs and/or have to communicate
# with other VMs, the corresponding settings will have to match these ones -
# think to all upper layers which may rely implicitly on that port information)
#
#FIREWALL_OPT :=
FIREWALL_OPT := --tcp-range 50000 55000 --epmd-port $(EPMD_PORT)


ifdef MAX_PROCESS_COUNT
	MAX_PROCESS_OPT := --max-process-count $(MAX_PROCESS_COUNT)
endif


ifdef ASYNC_THREAD_COUNT
	ASYNC_THREAD_OPT := --async-thread-count $(ASYNC_THREAD_COUNT)
endif



# 'BEAM_PATH_OPT = --beam-paths $(BEAM_DIRS)' would be shorter, but would
# prevent adding other options to the launcher afterwards:
# (now we use absolute directories - thanks to 'pwd' - instead of relative ones,
# as, otherwise, as soon as a process changes the current working directory, any
# further loading of a user-defined module, for example if used then for the
# first time, would fail).
#
BEAM_PATH_OPT = $(foreach dir,$(BEAM_DIRS),--beam-dir $$(pwd)/$(dir)) \
				$(foreach dir,$(ABS_BEAM_DIRS),--beam-dir $(dir))


OVERALL_PZ_OPT = $(foreach dir,$(BEAM_DIRS),-pz $(dir))


# Increases the distribution buffer busy limit (default is 1024):
# See http://erlang.org/doc/man/erl.html#+zdbbl.
#
# 1024*1024 = 1048576
BUSY_LIMIT_OPT := --busy-limit 1048576

SCALE_OPT := $(BUSY_LIMIT_OPT)


# To tell run_erl where the VM log files shall be written;
ifdef VM_LOG_DIR
	  LOG_DIR_OPT := --log-dir $(VM_LOG_DIR)
endif


# If wanting to force UTF-8 for filename encoding on a launched VM:
# (generally not useful)
#
#ENCODING_OPT := +fnu


# Removed:
#
#  --ln $(VM_TEST_NAME)
#
#  -c $(PROJECT_NAME), as not specifying it on the command line allows to
# select the one in ~/.erlang.cookie (if any), which is better
#
#  -v, as the verbose mode for the launcher led to too many lines being output,
# now that user-defined code paths are absolute, thus significantly longer
#
# OVERRIDDEN_OPT left for the user.
ERL_PARAMETERIZED_LAUNCHER = $(ERL_LAUNCHER) $(FQDN_OPT) $(FIREWALL_OPT)  \
							 $(MAX_PROCESS_OPT) $(ASYNC_THREAD_OPT)       \
							 $(BEAM_PATH_OPT) $(SCALE_OPT) $(COOKIE_OPT)  \
							 $(LOG_DIR_OPT) $(ENCODING_OPT) $(OVERRIDDEN_OPT)


# Previously the short names were preferred, as they seemed to resist to
# non-existing or, more probably, improperly configured, DNS systems.
#
# However other issues may arise with them (ex: an inappropriate domain being
# taken by Erlang from /etc/resolv.conf), so we went back to the (supposedly
# more precise) long names.
#
# Then we realized that some use cases required the node name to be changed at
# runtime, implying the node should not be defined initially as a distributed
# one (hence the --nn option).
#
# This setting can also be overridden from the command-line (variable used in
# generic rules):
#
# (use net_utils:get_node_naming_mode/0 to detect the naming mode at runtime)
#
ifndef NODE_NAMING

	NODE_NAMING = "--ln"
	#NODE_NAMING = "--sn"
	#NODE_NAMING = "--nn"

endif


# Regarding the VM-specific command-line options, we used to rely on '-noshell'
# rather than on '-noinput', as no shell was wanted by default for executions,
# since executables are supposed to interact by themselves with the user.
# However, at least for some uses (ex: term_ui, relying on /bin/dialog), we
# *need* to have -noinput and not to have -noshell, while others (ex: text_ui,
# relying on io:get_line/1) require the opposite.
#
# So we finally designed a work-around (see system_utils:get_line/1) to stick
# with the option needed by term_ui.
#
# Note that setting a -noinput option implies that, for example when running a
# test, the user will not be able to use the Erlang shell afterwards (like done
# in test_facilities:finished/0).
#
# Moreover, should the Erlang program abort (whether using text_ui or term_ui),
# the initial (UNIX) shell from which the program was launched may malfunction a
# lot (many key presses being then lost), and despite countermeasures (such as
# Ceylan-Hull/reset-keyboard-mode.sh), we did not manage to restore it correctly
# in these cases, which is certainly a problem.
#
# Note that the '-start-verbatim-options' marker is used by the launch-erl.sh
# script in order to delimit the options it is supposed to recognize (before
# that marker) from the ones that shall be passed "as are" to the Erlang VM
# (after that marker), while the '-extra' marker delimits user/system Erlang
# flags from the application-specific command line arguments (which are
# specifically managed by executable_utils).
#
# So additional arguments FOO_ARGS to be passed to the VM while not being known
# of launch-erl.sh shall be specified between -start-verbatim-options and
# -extra, like in: '-start-verbatim-options $(FOO_ARGS) -extra'.


# In this case term_ui is ko (displays and terminates), text_ui is ok:
#EXEC_INTERNAL_OPTIONS := $(INTERNAL_OPTIONS) -start-verbatim-options -noinput -noshell

# The same:
#EXEC_INTERNAL_OPTIONS := $(INTERNAL_OPTIONS)

# term_ui is ok, text_ui by default has no input but with our workaround is ok -
# yet crashing may disrupt the hosting UNIX shell:
#
EXEC_INTERNAL_OPTIONS := $(INTERNAL_OPTIONS) -start-verbatim-options -noinput

# term_ui is ok, text_ui has no input:
#EXEC_INTERNAL_OPTIONS := $(INTERNAL_OPTIONS) -start-verbatim-options -noshell


# EXEC_EXTERNAL_OPTIONS referenced so that upper layers may add their own
# options (considered as belonging to the applicative realm rather than to the
# Erlang one, yet to be managed transparently for the user):
#
ALL_CMD_LINE_OPTIONS := $(EXEC_INTERNAL_OPTIONS) -extra $(EXEC_EXTERNAL_OPTIONS) $(CMD_LINE_OPT)


# Available in Ceylan-Hull:
DOC_TRACKER := $$(which track-make-automatic-source.sh)


ifdef PROJECT_ICON
	PROJECT_ICON_OPT := --icon-file $(PROJECT_ICON)
endif



# Release settings.


# Note: an awful gotcha with rebar3 is that it defines environment variables
# such as REBAR_BUILD_DIR, which can thus be used by default (and by mistake)
# typically by such make-based system running through rebar hooks.
#
# As a result: we should *never* define anymore any make variable prefixed with
# "REBAR"; we rely on the "MYRIAD_REBAR" prefix instead, unless we refer to a
# variable known to be set/used by rebar.
#
# Note: for clarity (typically for variables to be defined by upper layers),
# some variables are instead suffixed by "_FOR_REBAR" (ex: VERSION_FOR_REBAR).

# Toggle for additional logs (this is a rebar-specific variable):
REBAR_DEBUG =
#REBAR_DEBUG = DEBUG=1


# Relative to the usual root of this layer:
ifndef MYRIAD_REBAR_BUILD_ROOT
	MYRIAD_REBAR_BUILD_ROOT := _build
endif


# Default profile:

ifndef MYRIAD_REBAR_PROFILE
	MYRIAD_REBAR_PROFILE := default
endif

ifndef MYRIAD_REBAR_BUILD_DIR
	MYRIAD_REBAR_BUILD_DIR := $(MYRIAD_REBAR_BUILD_ROOT)/$(MYRIAD_REBAR_PROFILE)
endif


MYRIAD_REBAR_BUILD_BASE = $(MYRIAD_REBAR_BUILD_DIR)/lib/myriad


ifndef MYRIAD_REBAR_BUILD_BASE
	MYRIAD_REBAR_BUILD_BASE := $(MYRIAD_REBAR_BUILD_DIR)/lib/$(PROJECT_NAME_FOR_REBAR)
endif


ifndef MYRIAD_REBAR_FIND_SRC_OPT

	ifndef MYRIAD_REBAR_FIND_SRC_EXCLUDES

		# Do not insert spaces here:
		ifeq ($(PROJECT_NAME),Ceylan-Myriad)

			# For Myriad then:
			MYRIAD_REBAR_FIND_SRC_EXCLUDES := sql_support.erl hdf5_support.erl

		endif

		# Generated a relevant GNU find option for excluded sources:
		MYRIAD_REBAR_FIND_SRC_OPT := $(patsubst %,-a ! -name %,$(MYRIAD_REBAR_FIND_SRC_EXCLUDES))

	endif

endif


# Results of the filling of corresponding templates:
MYRIAD_REBAR_CONF_FILES := rebar.config                     \
						   rebar-for-hex.config             \
						   rebar-for-testing.config


MYRIAD_REBAR_TEST_DIR := /tmp


# To install rebar3: https://github.com/erlang/rebar3#getting-started
#
# Typically obtained thanks to:
#
# mkdir -p ~/Software && cd ~/Software && git clone
# https://github.com/erlang/rebar3.git && cd rebar3 && ./bootstrap
#
MYRIAD_REBAR_EXEC := $$(which rebar3 2>/dev/null)


# Elements for the generation of rebar.config:

# The modules to build first ('erl_first_files'):

# (the initial '../'s are removed, as this variable will be evaluated from the
# 'conf' directory)
#
MYRIAD_REBAR_FIRST_FILES := \"`echo $(patsubst ../%.beam,\"%.erl\",$(BOOTSTRAP_MODULES))|sed 's| |", "|g'`\"


# Transforming a recursively expanded variable into a simply expanded, otherwise
# later transformations will fail (cannot be nested):
#
# Note: is correct as happens last in the parsing process (after all layers, all
# GNUmake* files)
#
STABLE_INC := $(INC)


# Far from being straightforward (patsubst would fail due to nesting):
MYRIAD_REBAR_INCS := {i,\"`echo $(STABLE_INC)|sed 's| |\"}, {i,\"|g' | sed 's|-I\.\./||g'`\"}


# Useful to notify the user whenever a longer operation terminates:
NOTIFIER_TOOL := $$(which bong.sh 2>/dev/null || true)


# Some source files cannot be compiled in all cases (typically because they
# depend on the includes of prerequisites that may or may not be installed).
#
# As rebar will attempt blindly to compile them, we have to hide these files
# whenever necessary.
#
# (json_utils and related not listed here, as not needing specific includes)
#
UNSUPPORTED_SOURCES := src/data-management/sql_support.erl          \
					   src/data-management/sql_support_test.erl     \
					   src/data-management/hdf5_support.erl         \
					   src/data-management/hdf5_support_test.erl    \
					   src/data-management/raw_hdf5_test.erl


# For the sync-* targets:
ifndef SYNC_TOOL

	SYNC_TOOL := $$(which rsync 2>/dev/null)

	ifdef SSH_PORT
		SSH_OPT := -e "ssh -p $(SSH_PORT)"
	endif

	# Not -a, as it implies -o and -g (to preserve owner and groups), which is
	# not desirable here; not -t either, as having not enough permissions to
	# update time metadata:
	#
	SYNC_OPT := -rlpD -vz $(SSH_OPT)

endif



# Verification settings.


DIALYZER := $$(PATH=$(ERLANG_USUAL_ROOT)/bin:$$PATH which dialyzer 2>/dev/null)


# Options for Dialyzer, see http://www.erlang.org/doc/man/dialyzer.html:
# Could be added: -Wunderspecs (obsolete: -Wbehaviours)
DIALYZER_OPT := -Wrace_conditions


# To use a generic rule to produce it (unless already defined):
ifndef PLT_FILE
	PLT_FILE := $(PACKAGE_TOP)/$(PACKAGE_NAME).plt
endif

# To use a generic rule to produce it (unless already defined):
ifndef TYPE_LIST_FILE
	TYPE_LIST_FILE := $(PACKAGE_TOP)/declared-types-in-$(PACKAGE_NAME).txt
endif


# For later reuse in the next layer of the software stack:
MYRIAD_PLT_FILE := $(MYRIAD_TOP)/Ceylan-Myriad.plt


# Shorthand:
BATCH_OPT := 'CMD_LINE_OPT="--batch"'


# This is the merged PLT of the level just below in the software stack.
#
# For 'Myriad', it is the PLT of Erlang/OTP (possibly enriched by
# prerequisites):
#
ifndef PREDECESSOR_PLT
	PREDECESSOR_PLT = $(ERLANG_PLT_FILE)
endif


MAKE_CODE_STATS := $(MYRIAD_TOP)/src/scripts/make-code-stats.sh


# Not depending on any lower-level package.