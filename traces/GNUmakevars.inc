# Prerequisite: TRACES_TOP must be set.


# To understand why taking into account such conditionals requires to specify
# them first, and in each layer, rather than at the end of the last one, refer
# to the section about conditionals at the top of the GNUmakevars.inc of Myriad.


# Overall settings section.

#EXECUTION_TARGET = production

ifndef EXECUTION_TARGET

	# Other possible value: production
	EXECUTION_TARGET = development

endif


ifeq ($(EXECUTION_TARGET),development)

	#$(info Execution target is development)

	ENABLE_TRACES = true
	ENABLE_DEBUG = true

else ifeq ($(EXECUTION_TARGET),production)

	#$(info Execution target is production)

	ENABLE_TRACES = false
	ENABLE_DEBUG = false

else

	$(error Invalid Traces execution target '$(EXECUTION_TARGET)'; expecting either 'development' or 'production')

endif


# Enforce default values:


# To selectively force any (de)activation:
#ENABLE_TRACES := false

# Should already be set through EXECUTION_TARGET above:
ifndef ENABLE_TRACES
	ENABLE_TRACES := true
endif


# Tells whether traces will be activated for the next modules to be built.
# By default, they are enabled.
#
ifeq ($(ENABLE_TRACES),true)
	ENABLE_TRACE_OPT := -Dtracing_activated
else
	ENABLE_TRACE_OPT :=
endif



# Defining (optional) layer-specific flags, typically used by in-code
# conditional compilation primitives (see cond_utils for their management):

# Topics covered by flags:
#  - xx (none yet)

# Defining debug flags (at least the most verbose are by default commented-out):
ifeq ($(ENABLE_DEBUG),true)
	#TRACES_DEBUG_FLAGS += -Dtraces_debug_xx
endif

# Defining check flags:
ifeq ($(EXECUTION_TARGET),development)
	# Does not exist: TRACES_CHECK_FLAGS += -Dtraces_check_xx
endif

# As soon as Traces is used, no need to ellipse traces anymore, they can be
# fully preserved:
#
ERLANG_COMPILER_OPT_BASE += $(TRACES_DEBUG_FLAGS) \
							$(TRACES_CHECK_FLAGS) \
							-Dwooper_preserve_traces


# Useful to target for example the root of the current layer (ex: to locate the
# relevant, layer-local '_build' directory):
#
ifndef LAYER_TOP
	LAYER_TOP := $(TRACES_TOP)
endif


# Project section.

# PROJECT_NAME should be defined on a per-project basis.
ifndef PROJECT_NAME
	PROJECT_NAME := Ceylan-Traces
endif


# PROJECT_NAME_FOR_REBAR should be defined on a per-project basis.
ifndef PROJECT_NAME_FOR_REBAR
	PROJECT_NAME_FOR_REBAR := traces
endif


# The uniquely-defined version of this layer, for all uses (including rebar and
# hex):
#
TRACES_VERSION := 1.0.5


# PROJECT_VERSION should be defined on a per-project basis.
ifndef PROJECT_VERSION
	PROJECT_VERSION := $(TRACES_VERSION)
endif



ifndef PACKAGE_TOP
	PACKAGE_TOP := $(TRACES_TOP)
endif



# For any quick, local, non release-packaged update thereof:
WOOPER_CHECKOUT_BUILD = $(LAYER_TOP)/_checkouts/wooper
MWOOPER_SIBLING_BUILD = $(LAYER_TOP)/../wooper


# The OTP tree of the WOOPER application within a local OTP _build tree:
WOOPER_LOCAL_APP := $(LAYER_TOP)/_build/default/lib/wooper

# Where WOOPER BEAMs are to lie when used as an OTP application:
WOOPER_LOCAL_EBIN := $(WOOPER_LOCAL_APP)/ebin/



TRACES_RELEASE_BASE := Traces-$(PROJECT_VERSION)

TRACES_RELEASE_ARCHIVE_ZIP := $(TRACES_RELEASE_BASE).zip
TRACES_RELEASE_ARCHIVE_BZ2 := $(TRACES_RELEASE_BASE).tar.bz2
TRACES_RELEASE_ARCHIVE_XZ  := $(TRACES_RELEASE_BASE).tar.xz


# Source section.
TRACES_SRC := $(TRACES_TOP)/src

# BEAM path section.
TRACES_BEAM := $(TRACES_TOP)/src


# In an OTP/rebar3-style application layout, at compilation time, libraries
# making use of WOOPER will expect its includes to be located in:
#
# (Myriad already taken care of, at the WOOPER level)
#
WOOPER_OTP_INC := -I$(TRACES_TOP)/../wooper/include/


INC += $(WOOPER_OTP_INC)


# Defined so that full traces are kept in messages (as the user of Traces can
# properly consult longer ones):
#
# (note: only taken into account in an integrated build, i.e. when the WOOPER
# build happens whereas this GNUmakevars.inc has been included)
#
WOOPER_UNELLIPSED_TRACES_OPT= -Dwooper_unellipsed_traces


# See Myriad counterpart variable (MYRIAD_BEAM_DIRS) for explanations:
TRACES_BEAM_DIRS := $(TRACES_BEAM)


# We rely on Myriad and WOOPER as well:
BEAM_DIRS += $(TRACES_BEAM_DIRS)


ifndef VM_TEST_NAME
	VM_NAME := traces_debug
endif


ifndef VM_TEST_NAME
	VM_TEST_NAME := traces_test
endif



ERLANG_COMPILER_OPT_BASE += $(ENABLE_TRACE_OPT)


# Strangely not taken into account by the compiler, hence commented-out here and
# replaced by a in-file -compile/1 directive (in class_TraceEmitter.hrl):
#
# (same for {test,app}_trace_disabled/{1,2} and al)
#
#ERLANG_COMPILER_OPT_BASE += "+{nowarn_unused_function,[{trace_disabled,1},{trace_disabled,2},{trace_disabled,3},{trace_disabled,4},{trace_disabled,5},{test_trace_disabled,1},{test_trace_disabled,2}]}"


# Not wanting to trigger WOOPER-specific settings (ex: the modules bootstrapped
# for it, like wooper_info), hence we define the corresponding Make variables
# with an empty value (otherwise if using 'MY_VAR= ', MY_VAR would be deemed not
# defined); however of course most rules/variables still apply.


#ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES
#	ERLANG_COMPILER_PARSE_TRANSFORM_OPT_FOR_STANDARD_MODULES = $(empty)
#endif


ifndef PARSE_TRANSFORM_DIRS
	PARSE_TRANSFORM_DIRS = $(empty)
endif


#ifndef ERLANG_COMPILER_PARSE_TRANSFORM_OPT
#	ERLANG_COMPILER_PARSE_TRANSFORM_OPT = $(empty)
#endif


ifndef BOOTSTRAP_MODULES
	BOOTSTRAP_MODULES = $(empty)
endif



TRACES_FILES = $(wildcard *.traces)


AUX_GENERATED_FILES  := $(AUX_GENERATED_FILES)  \
	$(patsubst %.traces,%.aux,$(TRACES_FILES))

TEX_GENERATED_FILES  := $(TEX_GENERATED_FILES)  \
	$(patsubst %.traces,%.tex,$(TRACES_FILES))

OUT_GENERATED_FILES  := $(OUT_GENERATED_FILES)  \
	$(patsubst %.traces,%.out,$(TRACES_FILES))

LOG_GENERATED_FILES  := $(LOG_GENERATED_FILES)  \
	$(patsubst %.traces,%.log,$(TRACES_FILES))

HTML_GENERATED_FILES := $(HTML_GENERATED_FILES) \
	$(patsubst %.traces,%.html,$(TRACES_FILES))

PDF_GENERATED_FILES  := $(PDF_GENERATED_FILES)  \
	$(patsubst %.traces,%.pdf,$(TRACES_FILES))


TRACES_GENERATED_FILES := $(AUX_GENERATED_FILES) $(TEX_GENERATED_FILES)    \
	$(OUT_GENERATED_FILES) $(LOG_GENERATED_FILES) $(HTML_GENERATED_FILES)  \
	$(PDF_GENERATED_FILES) $(GENERATOR_LOG_FILE) Listener-*.traces


# Used by the clean-override target in Myriad:
FILES_TO_CLEAN += $(TRACES_FILES) $(TRACES_GENERATED_FILES)


# For later reuse in the next layer of the software stack:
TRACES_PLT_FILE := $(TRACES_TOP)/traces.plt


# This is the merged PLT of the level just below in the software stack.
# For Traces, it is the PLT of 'WOOPER':
#
ifndef PREDECESSOR_PLT
	PREDECESSOR_PLT = $(WOOPER_PLT_FILE)
endif


# We define the path to the WOOPER layer (possibly available as a symbolic
# link).
#
# For the sake of these build-related makefiles, Ceylan dependencies will be
# searched (in that order, the one enforced in otp_utils) in a local _checkouts
# child directory (if checkout directories are used and if the top-level package
# of interest is the current one), otherwise in sibling directories (i.e. whose
# build root is at the same level as this one).
#
# This last case happens notably if not using checkout directories, or if using
# them and if all the packages mentioned here are themselves dependencies (in
# which case they are all sitting together in the _checkouts child directory).
#
# Note also we rely on the names of build roots (ex: myriad) as opposed to
# project/repository names (ex: Ceylan-Myriad):
#
# First priority is an existing checkout directory:
ifneq ($(wildcard $(TRACES_TOP)/_checkouts/wooper/.),)
	WOOPER_TOP = $(TRACES_TOP)/_checkouts/wooper
else
	# Second, a sibling one is searched for:
	ifneq ($(wildcard $(TRACES_TOP)/../wooper/.),)
		WOOPER_TOP = $(TRACES_TOP)/../wooper
   else
		# Otherwise most probably in a 'rebar3 compile' context:
		ifneq ($(wildcard $(TRACES_TOP)/_build/default/lib/wooper/.),)
			WOOPER_TOP = $(TRACES_TOP)/_build/default/lib/wooper
		else
			FORCE_EVAL := $(error Unable to find the build root of Ceylan-WOOPER from $(realpath .).)
		endif
	endif
endif
